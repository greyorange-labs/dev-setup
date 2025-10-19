/**
 * MCP Resources for retrieving development instructions
 */

import type { InstructionsDatabase } from './db.js';
import type { ProjectContext } from './types.js';

export function createResources(db: InstructionsDatabase) {
    return {
        /**
         * Get workflow rules for the 4-phase development process
         */
        'instructions://workflow-rules': {
            description: 'Get workflow rules for the 4-phase development process (clarification, planning, implementation, review)',
            handler: async () => {
                const instructions = db.searchInstructions({
                    category: 'workflow-rules',
                });

                return {
                    contents: [
                        {
                            uri: 'instructions://workflow-rules',
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, 'Workflow Rules'),
                        },
                    ],
                };
            },
        },

        /**
         * Get coding patterns for a specific language
         */
        'instructions://coding-patterns/{language}': {
            description: 'Get coding patterns for a specific programming language (erlang, java, typescript, python, etc.)',
            handler: async (uri: string) => {
                const language = extractLanguageFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'coding-patterns',
                    language,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `${language.toUpperCase()} Coding Patterns`),
                        },
                    ],
                };
            },
        },

        /**
         * Get project-specific configuration
         */
        'instructions://project-config/{project}': {
            description: 'Get project-specific configuration and guidelines',
            handler: async (uri: string) => {
                const project = extractProjectFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'project-configs',
                    project,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `${project} Project Configuration`),
                        },
                    ],
                };
            },
        },

        /**
         * Get common pitfalls for a language or project
         */
        'instructions://pitfalls/{context}': {
            description: 'Get common pitfalls and anti-patterns for a language or project',
            handler: async (uri: string) => {
                const context = extractContextFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'pitfalls',
                    language: context,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `Common Pitfalls - ${context}`),
                        },
                    ],
                };
            },
        },

        /**
         * Get component-specific guidelines
         */
        'instructions://component-guidelines/{project}': {
            description: 'Get component-specific guidelines for a project',
            handler: async (uri: string) => {
                const project = extractProjectFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'component-guidelines',
                    project,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `${project} Component Guidelines`),
                        },
                    ],
                };
            },
        },

        /**
         * Get testing strategies
         */
        'instructions://testing-strategies/{language}': {
            description: 'Get testing strategies for a specific language or framework',
            handler: async (uri: string) => {
                const language = extractLanguageFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'testing-strategies',
                    language,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `${language.toUpperCase()} Testing Strategies`),
                        },
                    ],
                };
            },
        },

        /**
         * Get architecture patterns
         */
        'instructions://architecture-patterns/{language}': {
            description: 'Get architecture patterns for a specific language or framework',
            handler: async (uri: string) => {
                const language = extractLanguageFromUri(uri);
                const instructions = db.searchInstructions({
                    category: 'architecture-patterns',
                    language,
                });

                return {
                    contents: [
                        {
                            uri,
                            mimeType: 'text/markdown',
                            text: formatInstructions(instructions, `${language.toUpperCase()} Architecture Patterns`),
                        },
                    ],
                };
            },
        },

        /**
         * Get context-aware instructions based on current project and file
         */
        'instructions://context-aware': {
            description: 'Get context-aware instructions based on the current project and file being worked on',
            handler: async (_uri: string, context?: ProjectContext) => {
                if (!context) {
                    return {
                        contents: [
                            {
                                uri: 'instructions://context-aware',
                                mimeType: 'text/markdown',
                                text: '# No Context Available\n\nUnable to determine current project context.',
                            },
                        ],
                    };
                }

                const instructions = db.searchInstructions({
                    project: context.projectName,
                    language: context.language,
                });

                return {
                    contents: [
                        {
                            uri: 'instructions://context-aware',
                            mimeType: 'text/markdown',
                            text: formatContextAwareInstructions(instructions, context),
                        },
                    ],
                };
            },
        },
    };
}

function formatInstructions(instructions: any[], title: string): string {
    if (instructions.length === 0) {
        return `# ${title}\n\nNo instructions found.`;
    }

    let markdown = `# ${title}\n\n`;
    markdown += `_Found ${instructions.length} instruction(s)_\n\n`;
    markdown += '---\n\n';

    for (const instruction of instructions) {
        markdown += `## ${instruction.title}\n\n`;

        // Add metadata
        const metadata: string[] = [];
        if (instruction.language) metadata.push(`**Language:** ${instruction.language}`);
        if (instruction.project) metadata.push(`**Project:** ${instruction.project}`);
        if (instruction.priority > 0) metadata.push(`**Priority:** ${instruction.priority}`);
        if (instruction.tags && instruction.tags.length > 0) {
            metadata.push(`**Tags:** ${instruction.tags.join(', ')}`);
        }

        if (metadata.length > 0) {
            markdown += metadata.join(' | ') + '\n\n';
        }

        markdown += instruction.content + '\n\n';
        markdown += '---\n\n';
    }

    return markdown;
}

function formatContextAwareInstructions(instructions: any[], context: ProjectContext): string {
    let markdown = `# Context-Aware Instructions\n\n`;
    markdown += `**Project:** ${context.projectName}\n`;
    markdown += `**Language:** ${context.language}\n`;
    if (context.framework) markdown += `**Framework:** ${context.framework}\n`;
    if (context.currentFile) markdown += `**Current File:** ${context.currentFile}\n`;
    markdown += '\n---\n\n';

    if (instructions.length === 0) {
        markdown += 'No specific instructions found for this context.\n';
        return markdown;
    }

    // Group by category
    const grouped = instructions.reduce((acc, inst) => {
        if (!acc[inst.category]) {
            acc[inst.category] = [];
        }
        acc[inst.category].push(inst);
        return acc;
    }, {} as Record<string, any[]>);

    for (const [category, items] of Object.entries(grouped)) {
        markdown += `## ${formatCategoryName(category)}\n\n`;

        for (const instruction of items as any[]) {
            markdown += `### ${instruction.title}\n\n`;
            markdown += instruction.content + '\n\n';
        }
    }

    return markdown;
}

function formatCategoryName(category: string): string {
    return category
        .split('-')
        .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
        .join(' ');
}

function extractLanguageFromUri(uri: string): string {
    const match = uri.match(/\/([^/]+)$/);
    return match ? match[1] : '';
}

function extractProjectFromUri(uri: string): string {
    const match = uri.match(/\/([^/]+)$/);
    return match ? match[1] : '';
}

function extractContextFromUri(uri: string): string {
    const match = uri.match(/\/([^/]+)$/);
    return match ? match[1] : '';
}
