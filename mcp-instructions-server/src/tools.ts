/**
 * MCP Tools for managing development instructions
 */

import { z } from 'zod';
import type { InstructionsDatabase } from './db.js';

// Zod schemas for tool parameters
const AddInstructionSchema = z.object({
    title: z.string().describe('Title of the instruction'),
    content: z.string().describe('Content of the instruction in markdown format'),
    category: z.enum([
        'workflow-rules',
        'coding-patterns',
        'component-guidelines',
        'pitfalls',
        'project-configs',
        'testing-strategies',
        'architecture-patterns',
    ]).describe('Category of the instruction'),
    language: z.string().optional().describe('Programming language this instruction applies to'),
    project: z.string().optional().describe('Project name this instruction applies to'),
    tags: z.array(z.string()).optional().describe('Tags for categorizing the instruction'),
    priority: z.number().optional().default(0).describe('Priority level (higher = more important)'),
});

const UpdateInstructionSchema = z.object({
    id: z.number().describe('ID of the instruction to update'),
    title: z.string().optional().describe('New title'),
    content: z.string().optional().describe('New content in markdown format'),
    category: z.enum([
        'workflow-rules',
        'coding-patterns',
        'component-guidelines',
        'pitfalls',
        'project-configs',
        'testing-strategies',
        'architecture-patterns',
    ]).optional().describe('New category'),
    language: z.string().optional().describe('New language'),
    project: z.string().optional().describe('New project'),
    tags: z.array(z.string()).optional().describe('New tags'),
    priority: z.number().optional().describe('New priority level'),
});

const DeleteInstructionSchema = z.object({
    id: z.number().describe('ID of the instruction to delete'),
});

const SearchInstructionsSchema = z.object({
    category: z.enum([
        'workflow-rules',
        'coding-patterns',
        'component-guidelines',
        'pitfalls',
        'project-configs',
        'testing-strategies',
        'architecture-patterns',
    ]).optional().describe('Filter by category'),
    language: z.string().optional().describe('Filter by programming language'),
    project: z.string().optional().describe('Filter by project name'),
    tags: z.array(z.string()).optional().describe('Filter by tags'),
    query: z.string().optional().describe('Full-text search query'),
    limit: z.number().optional().default(50).describe('Maximum number of results to return'),
});

const GetInstructionSchema = z.object({
    id: z.number().describe('ID of the instruction to retrieve'),
});

export function createTools(db: InstructionsDatabase) {
    return {
        add_instruction: {
            description: 'Add a new development instruction to the database',
            inputSchema: AddInstructionSchema,
            handler: async (params: z.infer<typeof AddInstructionSchema>) => {
                try {
                    const id = db.addInstruction(params);
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    id,
                                    message: `Instruction added successfully with ID: ${id}`,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        update_instruction: {
            description: 'Update an existing development instruction',
            inputSchema: UpdateInstructionSchema,
            handler: async (params: z.infer<typeof UpdateInstructionSchema>) => {
                try {
                    const success = db.updateInstruction(params);
                    if (success) {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: true,
                                        message: `Instruction ${params.id} updated successfully`,
                                    }, null, 2),
                                },
                            ],
                        };
                    } else {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: false,
                                        message: `Instruction ${params.id} not found`,
                                    }, null, 2),
                                },
                            ],
                            isError: true,
                        };
                    }
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        delete_instruction: {
            description: 'Delete a development instruction from the database',
            inputSchema: DeleteInstructionSchema,
            handler: async (params: z.infer<typeof DeleteInstructionSchema>) => {
                try {
                    const success = db.deleteInstruction(params.id);
                    if (success) {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: true,
                                        message: `Instruction ${params.id} deleted successfully`,
                                    }, null, 2),
                                },
                            ],
                        };
                    } else {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: false,
                                        message: `Instruction ${params.id} not found`,
                                    }, null, 2),
                                },
                            ],
                            isError: true,
                        };
                    }
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        search_instructions: {
            description: 'Search for development instructions by category, language, project, tags, or text query',
            inputSchema: SearchInstructionsSchema,
            handler: async (params: z.infer<typeof SearchInstructionsSchema>) => {
                try {
                    const instructions = db.searchInstructions(params);
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    count: instructions.length,
                                    instructions,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        get_instruction: {
            description: 'Get a specific instruction by ID',
            inputSchema: GetInstructionSchema,
            handler: async (params: z.infer<typeof GetInstructionSchema>) => {
                try {
                    const instruction = db.getInstructionById(params.id);
                    if (instruction) {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: true,
                                        instruction,
                                    }, null, 2),
                                },
                            ],
                        };
                    } else {
                        return {
                            content: [
                                {
                                    type: 'text',
                                    text: JSON.stringify({
                                        success: false,
                                        message: `Instruction ${params.id} not found`,
                                    }, null, 2),
                                },
                            ],
                            isError: true,
                        };
                    }
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        list_categories: {
            description: 'List all available instruction categories',
            inputSchema: z.object({}),
            handler: async () => {
                try {
                    const categories = db.getAllCategories();
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    categories,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        list_languages: {
            description: 'List all programming languages that have instructions',
            inputSchema: z.object({}),
            handler: async () => {
                try {
                    const languages = db.getAllLanguages();
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    languages,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        list_projects: {
            description: 'List all projects that have instructions',
            inputSchema: z.object({}),
            handler: async () => {
                try {
                    const projects = db.getAllProjects();
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    projects,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },

        list_tags: {
            description: 'List all tags used in instructions',
            inputSchema: z.object({}),
            handler: async () => {
                try {
                    const tags = db.getAllTags();
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: true,
                                    tags,
                                }, null, 2),
                            },
                        ],
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: 'text',
                                text: JSON.stringify({
                                    success: false,
                                    error: error instanceof Error ? error.message : 'Unknown error',
                                }, null, 2),
                            },
                        ],
                        isError: true,
                    };
                }
            },
        },
    };
}
