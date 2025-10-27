import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { InstructionsDatabase } from './db.js';
import { InstructionResource, FileNotFoundError } from './types.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export class ResourceManager {
    private db: InstructionsDatabase;

    constructor(database: InstructionsDatabase) {
        this.db = database;
    }

    async loadGlobalRules(): Promise<string> {
        try {
            // Global rules file is at same level as build/ directory
            const globalRulesPath = resolve(__dirname, '..', 'global-rules.md');
            const content = await readFile(globalRulesPath, 'utf-8');
            return content;
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
                throw new FileNotFoundError('global-rules.md');
            }
            throw error;
        }
    }

    listResources(): InstructionResource[] {
        const resources: InstructionResource[] = [
            {
                uri: 'instructions://global-rules',
                name: 'Global AI Assistant Rules',
                description: 'Complete set of 6 mandatory rules for disciplined AI coding workflows',
                mimeType: 'text/markdown'
            }
        ];

        // Add database instructions as resources
        try {
            const instructions = this.db.getAllInstructions();
            for (const instruction of instructions) {
                resources.push({
                    uri: `instructions://${instruction.name}`,
                    name: instruction.name,
                    description: instruction.description || `${instruction.category} instruction`,
                    mimeType: 'text/markdown'
                });
            }
        } catch (error) {
            console.error('Failed to load database instructions:', error);
        }

        return resources;
    }

    async readResource(uri: string): Promise<{ content: string; mimeType: string }> {
        if (uri === 'instructions://global-rules') {
            const content = await this.loadGlobalRules();
            return { content, mimeType: 'text/markdown' };
        }

        // Handle database instructions
        if (uri.startsWith('instructions://')) {
            const name = uri.replace('instructions://', '');
            const instruction = this.db.getInstructionByName(name);

            if (!instruction) {
                throw new FileNotFoundError(`instruction: ${name}`);
            }

            return {
                content: instruction.content,
                mimeType: 'text/markdown'
            };
        }

        throw new FileNotFoundError(`Unknown resource: ${uri}`);
    }

    async getResourceContent(uri: string): Promise<string> {
        const { content } = await this.readResource(uri);
        return content;
    }
}