#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
    CallToolRequestSchema,
    ListResourcesRequestSchema,
    ListToolsRequestSchema,
    ReadResourceRequestSchema,
    ListPromptsRequestSchema,
    GetPromptRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';

import { InstructionsDatabase } from './db.js';
import { ResourceManager } from './resources.js';
import { ToolManager } from './tools.js';
import {
    SearchInstructionsArgsSchema,
    AddInstructionArgsSchema,
    UpdateInstructionArgsSchema,
} from './types.js';
import { readFileSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

// ES module equivalents
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

class MCPInstructionsServer {
    private server: Server;
    private database: InstructionsDatabase;
    private resourceManager: ResourceManager;
    private toolManager: ToolManager;

    constructor() {
        this.server = new Server(
            {
                name: 'mcp-instructions-server',
                version: '1.0.0',
            },
            {
                capabilities: {
                    resources: {},
                    tools: {},
                    prompts: {},  // ADD PROMPTS CAPABILITY
                },
            }
        );

        // Initialize database and managers
        this.database = new InstructionsDatabase();
        this.resourceManager = new ResourceManager(this.database);
        this.toolManager = new ToolManager(this.database);

        this.setupHandlers();
    }

    private setupHandlers() {
        // List available resources
        this.server.setRequestHandler(ListResourcesRequestSchema, async () => {
            try {
                const resources = this.resourceManager.listResources();
                return { resources };
            } catch (error) {
                console.error('Error listing resources:', error);
                return { resources: [] };
            }
        });

        // Read specific resource content
        this.server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
            try {
                const { uri } = request.params;
                const { content, mimeType } = await this.resourceManager.readResource(uri);

                return {
                    contents: [
                        {
                            uri,
                            mimeType,
                            text: content,
                        },
                    ],
                };
            } catch (error) {
                console.error(`Error reading resource ${request.params.uri}:`, error);
                throw new Error(`Failed to read resource: ${error instanceof Error ? error.message : 'Unknown error'}`);
            }
        });

        // List available tools
        this.server.setRequestHandler(ListToolsRequestSchema, async () => {
            return {
                tools: [
                    {
                        name: 'search_instructions',
                        description: 'Search for instructions by query and optional category filter',
                        inputSchema: SearchInstructionsArgsSchema,
                    },
                    {
                        name: 'add_instruction',
                        description: 'Add a new instruction to the database',
                        inputSchema: AddInstructionArgsSchema,
                    },
                    {
                        name: 'update_instruction',
                        description: 'Update an existing instruction',
                        inputSchema: UpdateInstructionArgsSchema,
                    },
                    {
                        name: 'list_instructions',
                        description: 'List all instructions in the database',
                        inputSchema: {
                            type: 'object',
                            properties: {},
                        },
                    },
                    {
                        name: 'get_instruction',
                        description: 'Get a specific instruction by ID',
                        inputSchema: {
                            type: 'object',
                            properties: {
                                id: {
                                    type: 'number',
                                    description: 'The ID of the instruction to retrieve',
                                },
                            },
                            required: ['id'],
                        },
                    },
                    {
                        name: 'delete_instruction',
                        description: 'Delete an instruction by ID',
                        inputSchema: {
                            type: 'object',
                            properties: {
                                id: {
                                    type: 'number',
                                    description: 'The ID of the instruction to delete',
                                },
                            },
                            required: ['id'],
                        },
                    },
                    {
                        name: 'get_categories',
                        description: 'Get all available instruction categories',
                        inputSchema: {
                            type: 'object',
                            properties: {},
                        },
                    },
                ],
            };
        });

        // Handle tool calls
        this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
            const { name, arguments: args } = request.params;

            try {
                switch (name) {
                    case 'search_instructions': {
                        const validatedArgs = SearchInstructionsArgsSchema.parse(args);
                        return this.toolManager.searchInstructions(validatedArgs);
                    }

                    case 'add_instruction': {
                        const validatedArgs = AddInstructionArgsSchema.parse(args);
                        return this.toolManager.addInstruction(validatedArgs);
                    }

                    case 'update_instruction': {
                        const validatedArgs = UpdateInstructionArgsSchema.parse(args);
                        return this.toolManager.updateInstruction(validatedArgs);
                    }

                    case 'list_instructions': {
                        return this.toolManager.listInstructions();
                    }

                    case 'get_instruction': {
                        const id = Number(args?.id);
                        if (isNaN(id)) {
                            throw new Error('Invalid instruction ID');
                        }
                        return this.toolManager.getInstructionById(id);
                    }

                    case 'delete_instruction': {
                        const id = Number(args?.id);
                        if (isNaN(id)) {
                            throw new Error('Invalid instruction ID');
                        }
                        return this.toolManager.deleteInstruction(id);
                    }

                    case 'get_categories': {
                        return this.toolManager.getCategories();
                    }

                    default:
                        throw new Error(`Unknown tool: ${name}`);
                }
            } catch (error) {
                console.error(`Error executing tool ${name}:`, error);
                return {
                    content: [
                        {
                            type: 'text',
                            text: `Error: ${error instanceof Error ? error.message : 'Unknown error'}`,
                        },
                    ],
                    isError: true,
                };
            }
        });

        // List available prompts (CRITICAL FOR RULE ENFORCEMENT)
        this.server.setRequestHandler(ListPromptsRequestSchema, async () => {
            return {
                prompts: [
                    {
                        name: 'enforce-coding-rules',
                        description: 'Enforces the 6 critical coding rules: requirement clarification, planning, validation, no auto-docs/tests',
                        arguments: [],
                    },
                    {
                        name: 'start-conversation',
                        description: 'Start a new coding conversation with all rules automatically loaded',
                        arguments: [],
                    },
                ],
            };
        });

        // Get specific prompt content (THIS IS WHERE RULES ARE INJECTED)
        this.server.setRequestHandler(GetPromptRequestSchema, async (request) => {
            const { name } = request.params;

            try {
                // Load global rules from file
                const globalRulesPath = resolve(__dirname, '..', 'global-rules.md');
                const rulesContent = readFileSync(globalRulesPath, 'utf-8');

                console.error(`[MCP] Injecting rules into conversation via prompt: ${name}`);

                if (name === 'enforce-coding-rules' || name === 'start-conversation') {
                    return {
                        messages: [
                            {
                                role: 'user',
                                content: {
                                    type: 'text',
                                    text: `CRITICAL SYSTEM INSTRUCTIONS - READ BEFORE RESPONDING:

${rulesContent}

---

YOU MUST FOLLOW THESE RULES FOR EVERY RESPONSE:
1. When user asks for ANYTHING, start with requirement clarification (restate, assumptions, questions)
2. After confirmation, create detailed plan with NO code
3. After plan approval, implement following the plan
4. After implementation, ASK about documentation and tests

DO NOT SKIP ANY RULE. DO NOT IMPLEMENT WITHOUT APPROVAL.

Now respond to the user's request following Rule 1 (Requirement Clarification).`,
                                },
                            },
                        ],
                    };
                }

                throw new Error(`Unknown prompt: ${name}`);
            } catch (error) {
                console.error(`Error getting prompt ${name}:`, error);
                throw new Error(`Failed to get prompt: ${error instanceof Error ? error.message : 'Unknown error'}`);
            }
        });
    }

    async start() {
        try {
            const transport = new StdioServerTransport();
            await this.server.connect(transport);
            console.error('MCP Instructions Server started successfully');
        } catch (error) {
            console.error('Failed to start MCP server:', error);
            process.exit(1);
        }
    }

    async stop() {
        try {
            this.database.close();
            await this.server.close();
            console.error('MCP Instructions Server stopped');
        } catch (error) {
            console.error('Error stopping server:', error);
        }
    }
}

// Handle graceful shutdown
process.on('SIGINT', async () => {
    console.error('Received SIGINT, shutting down gracefully...');
    if (server) {
        await server.stop();
    }
    process.exit(0);
});

process.on('SIGTERM', async () => {
    console.error('Received SIGTERM, shutting down gracefully...');
    if (server) {
        await server.stop();
    }
    process.exit(0);
});

// Start the server
const server = new MCPInstructionsServer();
server.start().catch(error => {
    console.error('Server startup failed:', error);
    process.exit(1);
});