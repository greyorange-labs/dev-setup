import { readFile } from 'fs/promises';
import { readdir } from 'fs/promises';
import { fileURLToPath } from 'url';
import { dirname, resolve, extname } from 'path';
import { InstructionsDatabase } from './db.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

interface SeedInstruction {
    name: string;
    content: string;
    category: string;
    description?: string;
}

async function loadInstructionsFromJSON(): Promise<SeedInstruction[]> {
    const instructionsDir = resolve(__dirname, '..', 'instructions');
    const instructions: SeedInstruction[] = [];

    try {
        const files = await readdir(instructionsDir);

        for (const file of files) {
            if (extname(file) === '.json') {
                try {
                    const filePath = resolve(instructionsDir, file);
                    const content = await readFile(filePath, 'utf-8');
                    const data = JSON.parse(content);

                    if (Array.isArray(data)) {
                        instructions.push(...data);
                    } else if (data.instructions && Array.isArray(data.instructions)) {
                        instructions.push(...data.instructions);
                    } else {
                        console.warn(`Invalid format in ${file}: expected array or object with 'instructions' array`);
                    }
                } catch (error) {
                    console.warn(`Failed to load ${file}:`, error);
                }
            }
        }
    } catch (error) {
        // Instructions directory doesn't exist or is empty
        console.log('No instructions directory found or no JSON files to load');
    }

    return instructions;
}

async function seedDatabase() {
    console.log('🌱 Seeding instructions database...');

    const db = new InstructionsDatabase();

    try {
        // Load instructions from JSON files
        const instructions = await loadInstructionsFromJSON();

        if (instructions.length === 0) {
            console.log('No instructions found to seed');
            return;
        }

        let added = 0;
        let skipped = 0;

        for (const instruction of instructions) {
            try {
                // Check if instruction already exists
                const existing = db.getInstructionByName(instruction.name);

                if (existing) {
                    console.log(`⏭️  Skipping existing instruction: ${instruction.name}`);
                    skipped++;
                    continue;
                }

                // Add new instruction
                const id = db.addInstruction(instruction);
                console.log(`✅ Added instruction: ${instruction.name} (ID: ${id})`);
                added++;
            } catch (error) {
                console.error(`❌ Failed to add instruction '${instruction.name}':`, error);
            }
        }

        console.log(`\n📊 Seeding complete:`);
        console.log(`   Added: ${added} instructions`);
        console.log(`   Skipped: ${skipped} existing instructions`);
        console.log(`   Total in database: ${db.getAllInstructions().length} instructions`);

    } catch (error) {
        console.error('❌ Seeding failed:', error);
        process.exit(1);
    } finally {
        db.close();
    }
}

// Run seeding if this script is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    seedDatabase().catch(error => {
        console.error('Seeding process failed:', error);
        process.exit(1);
    });
}