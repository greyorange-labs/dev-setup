/**
 * SQLite database setup and operations for the MCP Instructions Server
 */

import Database from 'better-sqlite3';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import type {
    Instruction,
    AddInstructionParams,
    UpdateInstructionParams,
    SearchInstructionsParams,
} from './types.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const DB_PATH = join(__dirname, '..', 'instructions.db');

export class InstructionsDatabase {
    private db: Database.Database;

    constructor(dbPath: string = DB_PATH) {
        this.db = new Database(dbPath);
        this.db.pragma('journal_mode = WAL');
        this.initializeTables();
    }

    private initializeTables(): void {
        // Create instructions table
        this.db.exec(`
      CREATE TABLE IF NOT EXISTS instructions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        title TEXT NOT NULL,
        content TEXT NOT NULL,
        category TEXT NOT NULL,
        language TEXT,
        project TEXT,
        priority INTEGER DEFAULT 0,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );
    `);

        // Create tags table
        this.db.exec(`
      CREATE TABLE IF NOT EXISTS tags (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        instruction_id INTEGER NOT NULL,
        tag TEXT NOT NULL,
        FOREIGN KEY (instruction_id) REFERENCES instructions(id) ON DELETE CASCADE
      );
    `);

        // Create indexes for better search performance
        this.db.exec(`
      CREATE INDEX IF NOT EXISTS idx_category ON instructions(category);
      CREATE INDEX IF NOT EXISTS idx_language ON instructions(language);
      CREATE INDEX IF NOT EXISTS idx_project ON instructions(project);
      CREATE INDEX IF NOT EXISTS idx_priority ON instructions(priority DESC);
      CREATE INDEX IF NOT EXISTS idx_tags_instruction ON tags(instruction_id);
      CREATE INDEX IF NOT EXISTS idx_tags_tag ON tags(tag);
    `);

        // Create full-text search virtual table
        this.db.exec(`
      CREATE VIRTUAL TABLE IF NOT EXISTS instructions_fts USING fts5(
        title, content, category, language, project, tags,
        content='instructions', content_rowid='id'
      );
    `);

        // Create triggers to keep FTS in sync
        this.db.exec(`
      CREATE TRIGGER IF NOT EXISTS instructions_ai AFTER INSERT ON instructions BEGIN
        INSERT INTO instructions_fts(rowid, title, content, category, language, project)
        VALUES (new.id, new.title, new.content, new.category, new.language, new.project);
      END;

      CREATE TRIGGER IF NOT EXISTS instructions_ad AFTER DELETE ON instructions BEGIN
        DELETE FROM instructions_fts WHERE rowid = old.id;
      END;

      CREATE TRIGGER IF NOT EXISTS instructions_au AFTER UPDATE ON instructions BEGIN
        UPDATE instructions_fts
        SET title = new.title,
            content = new.content,
            category = new.category,
            language = new.language,
            project = new.project
        WHERE rowid = new.id;
      END;
    `);
    }

    addInstruction(params: AddInstructionParams): number {
        const stmt = this.db.prepare(`
      INSERT INTO instructions (title, content, category, language, project, priority)
      VALUES (?, ?, ?, ?, ?, ?)
    `);

        const result = stmt.run(
            params.title,
            params.content,
            params.category,
            params.language || null,
            params.project || null,
            params.priority || 0
        );

        const instructionId = result.lastInsertRowid as number;

        // Add tags if provided
        if (params.tags && params.tags.length > 0) {
            const tagStmt = this.db.prepare(`
        INSERT INTO tags (instruction_id, tag) VALUES (?, ?)
      `);

            const insertMany = this.db.transaction((tags: string[]) => {
                for (const tag of tags) {
                    tagStmt.run(instructionId, tag);
                }
            });

            insertMany(params.tags);
        }

        return instructionId;
    }

    updateInstruction(params: UpdateInstructionParams): boolean {
        const fields: string[] = [];
        const values: any[] = [];

        if (params.title !== undefined) {
            fields.push('title = ?');
            values.push(params.title);
        }
        if (params.content !== undefined) {
            fields.push('content = ?');
            values.push(params.content);
        }
        if (params.category !== undefined) {
            fields.push('category = ?');
            values.push(params.category);
        }
        if (params.language !== undefined) {
            fields.push('language = ?');
            values.push(params.language);
        }
        if (params.project !== undefined) {
            fields.push('project = ?');
            values.push(params.project);
        }
        if (params.priority !== undefined) {
            fields.push('priority = ?');
            values.push(params.priority);
        }

        if (fields.length === 0) {
            return false;
        }

        fields.push('updated_at = CURRENT_TIMESTAMP');
        values.push(params.id);

        const stmt = this.db.prepare(`
      UPDATE instructions SET ${fields.join(', ')} WHERE id = ?
    `);

        const result = stmt.run(...values);

        // Update tags if provided
        if (params.tags !== undefined) {
            // Delete existing tags
            this.db.prepare('DELETE FROM tags WHERE instruction_id = ?').run(params.id);

            // Add new tags
            if (params.tags.length > 0) {
                const tagStmt = this.db.prepare(`
          INSERT INTO tags (instruction_id, tag) VALUES (?, ?)
        `);

                const insertMany = this.db.transaction((tags: string[]) => {
                    for (const tag of tags) {
                        tagStmt.run(params.id, tag);
                    }
                });

                insertMany(params.tags);
            }
        }

        return result.changes > 0;
    }

    deleteInstruction(id: number): boolean {
        const stmt = this.db.prepare('DELETE FROM instructions WHERE id = ?');
        const result = stmt.run(id);
        return result.changes > 0;
    }

    searchInstructions(params: SearchInstructionsParams): Instruction[] {
        let query = `
      SELECT DISTINCT i.*, GROUP_CONCAT(t.tag) as tags
      FROM instructions i
      LEFT JOIN tags t ON i.id = t.instruction_id
    `;

        const conditions: string[] = [];
        const values: any[] = [];

        if (params.category) {
            conditions.push('i.category = ?');
            values.push(params.category);
        }

        if (params.language) {
            conditions.push('i.language = ?');
            values.push(params.language);
        }

        if (params.project) {
            conditions.push('i.project = ?');
            values.push(params.project);
        }

        if (params.tags && params.tags.length > 0) {
            const tagPlaceholders = params.tags.map(() => '?').join(',');
            conditions.push(`i.id IN (SELECT instruction_id FROM tags WHERE tag IN (${tagPlaceholders}))`);
            values.push(...params.tags);
        }

        if (params.query) {
            // Use FTS for text search
            conditions.push(`i.id IN (SELECT rowid FROM instructions_fts WHERE instructions_fts MATCH ?)`);
            values.push(params.query);
        }

        if (conditions.length > 0) {
            query += ' WHERE ' + conditions.join(' AND ');
        }

        query += ' GROUP BY i.id ORDER BY i.priority DESC, i.updated_at DESC';

        if (params.limit) {
            query += ' LIMIT ?';
            values.push(params.limit);
        }

        const stmt = this.db.prepare(query);
        const rows = stmt.all(...values) as any[];

        return rows.map((row) => ({
            id: row.id,
            title: row.title,
            content: row.content,
            category: row.category,
            language: row.language,
            project: row.project,
            tags: row.tags ? row.tags.split(',') : [],
            priority: row.priority,
            created_at: row.created_at,
            updated_at: row.updated_at,
        }));
    }

    getInstructionById(id: number): Instruction | null {
        const stmt = this.db.prepare(`
      SELECT i.*, GROUP_CONCAT(t.tag) as tags
      FROM instructions i
      LEFT JOIN tags t ON i.id = t.instruction_id
      WHERE i.id = ?
      GROUP BY i.id
    `);

        const row = stmt.get(id) as any;

        if (!row) {
            return null;
        }

        return {
            id: row.id,
            title: row.title,
            content: row.content,
            category: row.category,
            language: row.language,
            project: row.project,
            tags: row.tags ? row.tags.split(',') : [],
            priority: row.priority,
            created_at: row.created_at,
            updated_at: row.updated_at,
        };
    }

    getAllCategories(): string[] {
        const stmt = this.db.prepare('SELECT DISTINCT category FROM instructions ORDER BY category');
        const rows = stmt.all() as { category: string }[];
        return rows.map((row) => row.category);
    }

    getAllLanguages(): string[] {
        const stmt = this.db.prepare('SELECT DISTINCT language FROM instructions WHERE language IS NOT NULL ORDER BY language');
        const rows = stmt.all() as { language: string }[];
        return rows.map((row) => row.language);
    }

    getAllProjects(): string[] {
        const stmt = this.db.prepare('SELECT DISTINCT project FROM instructions WHERE project IS NOT NULL ORDER BY project');
        const rows = stmt.all() as { project: string }[];
        return rows.map((row) => row.project);
    }

    getAllTags(): string[] {
        const stmt = this.db.prepare('SELECT DISTINCT tag FROM tags ORDER BY tag');
        const rows = stmt.all() as { tag: string }[];
        return rows.map((row) => row.tag);
    }

    close(): void {
        this.db.close();
    }
}
