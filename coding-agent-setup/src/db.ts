import Database from 'better-sqlite3';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { Instruction, DatabaseError } from './types.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export class InstructionsDatabase {
    private db: Database.Database;

    constructor(dbPath?: string) {
        const defaultPath = resolve(__dirname, '..', 'instructions.db');
        this.db = new Database(dbPath || defaultPath);
        this.initializeTables();
    }

    private initializeTables(): void {
        try {
            this.db.exec(`
        CREATE TABLE IF NOT EXISTS instructions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL UNIQUE,
          content TEXT NOT NULL,
          category TEXT NOT NULL,
          description TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );

        CREATE INDEX IF NOT EXISTS idx_instructions_category ON instructions(category);
        CREATE INDEX IF NOT EXISTS idx_instructions_name ON instructions(name);

        CREATE TRIGGER IF NOT EXISTS update_instructions_updated_at
        AFTER UPDATE ON instructions
        BEGIN
          UPDATE instructions SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
        END;
      `);
        } catch (error) {
            throw new DatabaseError('Failed to initialize database tables', error as Error);
        }
    }

    getAllInstructions(): Instruction[] {
        try {
            const stmt = this.db.prepare('SELECT * FROM instructions ORDER BY name');
            return stmt.all() as Instruction[];
        } catch (error) {
            throw new DatabaseError('Failed to fetch all instructions', error as Error);
        }
    }

    getInstructionById(id: number): Instruction | null {
        try {
            const stmt = this.db.prepare('SELECT * FROM instructions WHERE id = ?');
            return stmt.get(id) as Instruction | null;
        } catch (error) {
            throw new DatabaseError(`Failed to fetch instruction with id ${id}`, error as Error);
        }
    }

    getInstructionByName(name: string): Instruction | null {
        try {
            const stmt = this.db.prepare('SELECT * FROM instructions WHERE name = ?');
            return stmt.get(name) as Instruction | null;
        } catch (error) {
            throw new DatabaseError(`Failed to fetch instruction with name ${name}`, error as Error);
        }
    }

    searchInstructions(query: string, category?: string): Instruction[] {
        try {
            let sql = `
        SELECT * FROM instructions
        WHERE (name LIKE ? OR content LIKE ? OR description LIKE ?)
      `;
            const params: any[] = [`%${query}%`, `%${query}%`, `%${query}%`];

            if (category) {
                sql += ' AND category = ?';
                params.push(category);
            }

            sql += ' ORDER BY name';

            const stmt = this.db.prepare(sql);
            return stmt.all(...params) as Instruction[];
        } catch (error) {
            throw new DatabaseError('Failed to search instructions', error as Error);
        }
    }

    addInstruction(instruction: Omit<Instruction, 'id' | 'created_at' | 'updated_at'>): number {
        try {
            const stmt = this.db.prepare(`
        INSERT INTO instructions (name, content, category, description)
        VALUES (?, ?, ?, ?)
      `);

            const result = stmt.run(
                instruction.name,
                instruction.content,
                instruction.category,
                instruction.description || null
            );

            return result.lastInsertRowid as number;
        } catch (error) {
            if ((error as any).code === 'SQLITE_CONSTRAINT_UNIQUE') {
                throw new DatabaseError(`Instruction with name '${instruction.name}' already exists`);
            }
            throw new DatabaseError('Failed to add instruction', error as Error);
        }
    }

    updateInstruction(id: number, updates: Partial<Omit<Instruction, 'id' | 'created_at' | 'updated_at'>>): boolean {
        try {
            const fields = Object.keys(updates).filter(key => updates[key as keyof typeof updates] !== undefined);
            if (fields.length === 0) {
                return false;
            }

            const sql = `UPDATE instructions SET ${fields.map(field => `${field} = ?`).join(', ')} WHERE id = ?`;
            const values: any[] = fields.map(field => updates[field as keyof typeof updates]);
            values.push(id);

            const stmt = this.db.prepare(sql);
            const result = stmt.run(...values);

            return result.changes > 0;
        } catch (error) {
            if ((error as any).code === 'SQLITE_CONSTRAINT_UNIQUE') {
                throw new DatabaseError(`Instruction with name '${updates.name}' already exists`);
            }
            throw new DatabaseError(`Failed to update instruction with id ${id}`, error as Error);
        }
    }

    deleteInstruction(id: number): boolean {
        try {
            const stmt = this.db.prepare('DELETE FROM instructions WHERE id = ?');
            const result = stmt.run(id);
            return result.changes > 0;
        } catch (error) {
            throw new DatabaseError(`Failed to delete instruction with id ${id}`, error as Error);
        }
    }

    getCategories(): string[] {
        try {
            const stmt = this.db.prepare('SELECT DISTINCT category FROM instructions ORDER BY category');
            const rows = stmt.all() as { category: string }[];
            return rows.map(row => row.category);
        } catch (error) {
            throw new DatabaseError('Failed to fetch categories', error as Error);
        }
    }

    close(): void {
        this.db.close();
    }
}