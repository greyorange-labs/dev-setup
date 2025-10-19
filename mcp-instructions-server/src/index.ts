#!/usr/bin/env node

/**
 * MCP Instructions Server
 *
 * A Model Context Protocol server that stores and retrieves development instructions
 * to help GitHub Copilot provide better implementations with ~0 errors.
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
    CallToolRequestSchema,
    ListResourcesRequestSchema,
    ListToolsRequestSchema,
    ReadResourceRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import { InstructionsDatabase } from './db.js';
import { createResources } from './resources.js';
import { createTools } from './tools.js';
import type { ProjectContext } from './types.js';
import { existsSync } from 'fs';
import { resolve, basename } from 'path';

// Initialize database
const db = new InstructionsDatabase();

// Initialize MCP server
const server = new Server(
    {
        name: 'mcp-instructions-server',
        version: '1.0.0',
    },
    {
        capabilities: {
            resources: {},
            tools: {},
        },
    }
);

// Create resources and tools
const resources = createResources(db);
const tools = createTools(db);

/**
 * Detect project context from environment or file path
 */
function detectProjectContext(filePath?: string): ProjectContext | null {
    if (!filePath) {
        return null;
    }

    const absolutePath = resolve(filePath);

    // Detect Butler Server (Erlang)
    if (absolutePath.includes('butler_server_develop')) {
        return {
            projectPath: '/Users/amar.c/workspace/gm_core/butler_server_develop',
            projectName: 'butler-server',
            language: 'erlang',
            framework: 'OTP',
            currentFile: filePath,
        };
    }

    // Detect GreyMatter Platform (Java/Maven)
    if (absolutePath.includes('greymatter-platform')) {
        return {
            projectPath: '/Users/amar.c/workspace/greymatter-platform',
            projectName: 'greymatter-platform',
            language: 'java',
            framework: 'spring-boot',
            currentFile: filePath,
        };
    }

    // Generic detection based on file extension
    const ext = filePath.split('.').pop()?.toLowerCase();
    const languageMap: Record<string, string> = {
        erl: 'erlang',
        hrl: 'erlang',
        java: 'java',
        ts: 'typescript',
        tsx: 'typescript',
        js: 'javascript',
        jsx: 'javascript',
        py: 'python',
        rb: 'ruby',
        go: 'go',
        rs: 'rust',
        cpp: 'cpp',
        c: 'c',
        cs: 'csharp',
    };

    const language = ext ? languageMap[ext] : 'unknown';

    // Try to find project root
    let projectPath = absolutePath;
    let projectName = 'unknown';

    // Walk up to find project root markers
    const rootMarkers = ['.git', 'package.json', 'pom.xml', 'rebar.config', 'Cargo.toml'];
    for (let i = 0; i < 10; i++) {
        const parent = resolve(projectPath, '..');
        if (parent === projectPath) break;

        if (rootMarkers.some(marker => existsSync(resolve(parent, marker)))) {
            projectPath = parent;
            projectName = basename(parent);
            break;
        }

        projectPath = parent;
    }

    return {
        projectPath,
        projectName,
        language,
        currentFile: filePath,
    };
}

/**
 * List available resources
 */
server.setRequestHandler(ListResourcesRequestSchema, async () => {
    return {
        resources: [
            {
                uri: 'instructions://workflow-rules',
                name: 'Workflow Rules',
                description: 'Get workflow rules for the 4-phase development process',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://coding-patterns/{language}',
                name: 'Coding Patterns',
                description: 'Get coding patterns for a specific language',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://project-config/{project}',
                name: 'Project Configuration',
                description: 'Get project-specific configuration',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://pitfalls/{context}',
                name: 'Common Pitfalls',
                description: 'Get common pitfalls and anti-patterns',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://component-guidelines/{project}',
                name: 'Component Guidelines',
                description: 'Get component-specific guidelines',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://testing-strategies/{language}',
                name: 'Testing Strategies',
                description: 'Get testing strategies for a language',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://architecture-patterns/{language}',
                name: 'Architecture Patterns',
                description: 'Get architecture patterns for a language',
                mimeType: 'text/markdown',
            },
            {
                uri: 'instructions://context-aware',
                name: 'Context-Aware Instructions',
                description: 'Get instructions based on current project context',
                mimeType: 'text/markdown',
            },
        ],
    };
});

/**
 * Read a specific resource
 */
server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
    const uri = request.params.uri;

    // Try to detect context from environment
    const currentFile = process.env.VSCODE_CURRENT_FILE;
    const context = detectProjectContext(currentFile);

    // Find matching resource handler
    for (const [pattern, resource] of Object.entries(resources)) {
        if (uri.startsWith(pattern.split('{')[0]) || uri === pattern) {
            return await resource.handler(uri, context ?? undefined);
        }
    }

    throw new Error(`Resource not found: ${uri}`);
});

/**
 * List available tools
 */
server.setRequestHandler(ListToolsRequestSchema, async () => {
    return {
        tools: [
            {
                name: 'add_instruction',
                description: 'Add a new development instruction to the database',
                inputSchema: {
                    type: 'object',
                    properties: {
                        title: { type: 'string', description: 'Title of the instruction' },
                        content: { type: 'string', description: 'Content in markdown format' },
                        category: {
                            type: 'string',
                            enum: [
                                'workflow-rules',
                                'coding-patterns',
                                'component-guidelines',
                                'pitfalls',
                                'project-configs',
                                'testing-strategies',
                                'architecture-patterns',
                            ],
                            description: 'Category of the instruction',
                        },
                        language: { type: 'string', description: 'Programming language (optional)' },
                        project: { type: 'string', description: 'Project name (optional)' },
                        tags: { type: 'array', items: { type: 'string' }, description: 'Tags (optional)' },
                        priority: { type: 'number', description: 'Priority level (optional)', default: 0 },
                    },
                    required: ['title', 'content', 'category'],
                },
            },
            {
                name: 'update_instruction',
                description: 'Update an existing development instruction',
                inputSchema: {
                    type: 'object',
                    properties: {
                        id: { type: 'number', description: 'ID of the instruction to update' },
                        title: { type: 'string', description: 'New title (optional)' },
                        content: { type: 'string', description: 'New content (optional)' },
                        category: { type: 'string', description: 'New category (optional)' },
                        language: { type: 'string', description: 'New language (optional)' },
                        project: { type: 'string', description: 'New project (optional)' },
                        tags: { type: 'array', items: { type: 'string' }, description: 'New tags (optional)' },
                        priority: { type: 'number', description: 'New priority (optional)' },
                    },
                    required: ['id'],
                },
            },
            {
                name: 'delete_instruction',
                description: 'Delete a development instruction',
                inputSchema: {
                    type: 'object',
                    properties: {
                        id: { type: 'number', description: 'ID of the instruction to delete' },
                    },
                    required: ['id'],
                },
            },
            {
                name: 'search_instructions',
                description: 'Search for instructions by various criteria',
                inputSchema: {
                    type: 'object',
                    properties: {
                        category: { type: 'string', description: 'Filter by category (optional)' },
                        language: { type: 'string', description: 'Filter by language (optional)' },
                        project: { type: 'string', description: 'Filter by project (optional)' },
                        tags: { type: 'array', items: { type: 'string' }, description: 'Filter by tags (optional)' },
                        query: { type: 'string', description: 'Full-text search query (optional)' },
                        limit: { type: 'number', description: 'Max results (optional)', default: 50 },
                    },
                },
            },
            {
                name: 'get_instruction',
                description: 'Get a specific instruction by ID',
                inputSchema: {
                    type: 'object',
                    properties: {
                        id: { type: 'number', description: 'ID of the instruction' },
                    },
                    required: ['id'],
                },
            },
            {
                name: 'list_categories',
                description: 'List all available instruction categories',
                inputSchema: { type: 'object', properties: {} },
            },
            {
                name: 'list_languages',
                description: 'List all programming languages that have instructions',
                inputSchema: { type: 'object', properties: {} },
            },
            {
                name: 'list_projects',
                description: 'List all projects that have instructions',
                inputSchema: { type: 'object', properties: {} },
            },
            {
                name: 'list_tags',
                description: 'List all tags used in instructions',
                inputSchema: { type: 'object', properties: {} },
            },
        ],
    };
});

/**
 * Handle tool calls
 */
server.setRequestHandler(CallToolRequestSchema, async (request) => {
    const toolName = request.params.name;
    const tool = tools[toolName as keyof typeof tools];

    if (!tool) {
        throw new Error(`Tool not found: ${toolName}`);
    }

    return await tool.handler(request.params.arguments as any);
});

/**
 * Start the server
 */
async function main() {
    const transport = new StdioServerTransport();
    await server.connect(transport);

    // Log startup (will be captured by MCP client)
    console.error('MCP Instructions Server started successfully');
    console.error('Database ready at:', resolve(__dirname, '..', 'instructions.db'));
}

main().catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
});

// Graceful shutdown
process.on('SIGINT', () => {
    db.close();
    process.exit(0);
});

process.on('SIGTERM', () => {
    db.close();
    process.exit(0);
});
