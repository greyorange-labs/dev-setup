import { InstructionsDatabase } from './db.js';
import {
    SearchInstructionsArgs,
    AddInstructionArgs,
    UpdateInstructionArgs,
    InstructionNotFoundError
} from './types.js';

export class ToolManager {
    private db: InstructionsDatabase;

    constructor(database: InstructionsDatabase) {
        this.db = database;
    }

    searchInstructions(args: SearchInstructionsArgs) {
        try {
            const results = this.db.searchInstructions(args.query, args.category);

            return {
                content: [
                    {
                        type: "text",
                        text: JSON.stringify({
                            query: args.query,
                            category: args.category,
                            results: results.map(instruction => ({
                                id: instruction.id,
                                name: instruction.name,
                                category: instruction.category,
                                description: instruction.description,
                                preview: instruction.content.substring(0, 200) + (instruction.content.length > 200 ? '...' : '')
                            }))
                        }, null, 2)
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error searching instructions: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    addInstruction(args: AddInstructionArgs) {
        try {
            const id = this.db.addInstruction(args);
            const instruction = this.db.getInstructionById(id);

            return {
                content: [
                    {
                        type: "text",
                        text: `Successfully added instruction '${args.name}' with ID ${id}\n\n${JSON.stringify(instruction, null, 2)}`
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error adding instruction: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    updateInstruction(args: UpdateInstructionArgs) {
        try {
            const { id, ...updates } = args;
            const updated = this.db.updateInstruction(id, updates);

            if (!updated) {
                throw new InstructionNotFoundError(args.id);
            }

            const instruction = this.db.getInstructionById(args.id);

            return {
                content: [
                    {
                        type: "text",
                        text: `Successfully updated instruction with ID ${args.id}\n\n${JSON.stringify(instruction, null, 2)}`
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error updating instruction: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    listInstructions() {
        try {
            const instructions = this.db.getAllInstructions();

            return {
                content: [
                    {
                        type: "text",
                        text: JSON.stringify({
                            total: instructions.length,
                            instructions: instructions.map(instruction => ({
                                id: instruction.id,
                                name: instruction.name,
                                category: instruction.category,
                                description: instruction.description,
                                created_at: instruction.created_at,
                                updated_at: instruction.updated_at
                            }))
                        }, null, 2)
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error listing instructions: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    getInstructionById(id: number) {
        try {
            const instruction = this.db.getInstructionById(id);

            if (!instruction) {
                throw new InstructionNotFoundError(id);
            }

            return {
                content: [
                    {
                        type: "text",
                        text: JSON.stringify(instruction, null, 2)
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error getting instruction: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    deleteInstruction(id: number) {
        try {
            const deleted = this.db.deleteInstruction(id);

            if (!deleted) {
                throw new InstructionNotFoundError(id);
            }

            return {
                content: [
                    {
                        type: "text",
                        text: `Successfully deleted instruction with ID ${id}`
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error deleting instruction: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }

    getCategories() {
        try {
            const categories = this.db.getCategories();

            return {
                content: [
                    {
                        type: "text",
                        text: JSON.stringify({
                            categories: categories
                        }, null, 2)
                    }
                ]
            };
        } catch (error) {
            return {
                content: [
                    {
                        type: "text",
                        text: `Error getting categories: ${error instanceof Error ? error.message : 'Unknown error'}`
                    }
                ],
                isError: true
            };
        }
    }
}