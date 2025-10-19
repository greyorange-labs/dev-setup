/**
 * Seed script to populate the database with initial instructions
 */

import { InstructionsDatabase } from './db.js';
import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const INSTRUCTIONS_DIR = join(__dirname, '..', 'instructions');

interface SeedInstruction {
    title: string;
    content: string;
    category: string;
    language?: string;
    project?: string;
    tags?: string[];
    priority?: number;
}

async function seedDatabase() {
    console.log('🌱 Seeding database with instructions...\n');

    const db = new InstructionsDatabase();

    const files = [
        'workflow-rules.json',
        'erlang-patterns.json',
        'java-patterns.json',
        'common-pitfalls.json',
        'project-configs.json',
    ];

    let totalAdded = 0;

    for (const file of files) {
        const filePath = join(INSTRUCTIONS_DIR, file);
        console.log(`📄 Loading ${file}...`);

        try {
            const content = readFileSync(filePath, 'utf-8');
            const instructions: SeedInstruction[] = JSON.parse(content);

            for (const instruction of instructions) {
                try {
                    const id = db.addInstruction({
                        title: instruction.title,
                        content: instruction.content,
                        category: instruction.category as any,
                        language: instruction.language,
                        project: instruction.project,
                        tags: instruction.tags || [],
                        priority: instruction.priority || 0,
                    });
                    totalAdded++;
                    console.log(`  ✅ Added: ${instruction.title} (ID: ${id})`);
                } catch (error) {
                    console.error(`  ❌ Failed to add: ${instruction.title}`, error);
                }
            }

            console.log(`  📊 Added ${instructions.length} instructions from ${file}\n`);
        } catch (error) {
            console.error(`❌ Error loading ${file}:`, error);
        }
    }

    // Print summary
    console.log('\n📊 Database Summary:');
    console.log('='.repeat(50));
    console.log(`Total instructions added: ${totalAdded}`);

    const categories = db.getAllCategories();
    console.log(`\nCategories (${categories.length}):`);
    categories.forEach(cat => {
        const count = db.searchInstructions({ category: cat as any }).length;
        console.log(`  - ${cat}: ${count} instructions`);
    });

    const languages = db.getAllLanguages();
    console.log(`\nLanguages (${languages.length}):`);
    languages.forEach(lang => {
        const count = db.searchInstructions({ language: lang }).length;
        console.log(`  - ${lang}: ${count} instructions`);
    });

    const projects = db.getAllProjects();
    console.log(`\nProjects (${projects.length}):`);
    projects.forEach(proj => {
        const count = db.searchInstructions({ project: proj }).length;
        console.log(`  - ${proj}: ${count} instructions`);
    });

    console.log('\n✨ Database seeding completed successfully!');

    db.close();
}

// Run seeding
seedDatabase().catch((error) => {
    console.error('Fatal error during seeding:', error);
    process.exit(1);
});
