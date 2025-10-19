/**
 * Type definitions for the MCP Instructions Server
 */

export interface Instruction {
    id?: number;
    title: string;
    content: string;
    category: InstructionCategory;
    language?: string;
    project?: string;
    tags: string[];
    priority: number;
    created_at?: string;
    updated_at?: string;
}

export type InstructionCategory =
    | 'workflow-rules'
    | 'coding-patterns'
    | 'component-guidelines'
    | 'pitfalls'
    | 'project-configs'
    | 'testing-strategies'
    | 'architecture-patterns';

export interface SearchFilters {
    category?: InstructionCategory;
    language?: string;
    project?: string;
    tags?: string[];
    query?: string;
}

export interface ProjectContext {
    projectPath: string;
    projectName: string;
    language: string;
    framework?: string;
    currentFile?: string;
}

export interface WorkflowPhase {
    phase: 'clarification' | 'planning' | 'implementation' | 'review';
    rules: string[];
    checkpoints: string[];
}

export interface AddInstructionParams {
    title: string;
    content: string;
    category: InstructionCategory;
    language?: string;
    project?: string;
    tags?: string[];
    priority?: number;
}

export interface UpdateInstructionParams {
    id: number;
    title?: string;
    content?: string;
    category?: InstructionCategory;
    language?: string;
    project?: string;
    tags?: string[];
    priority?: number;
}

export interface DeleteInstructionParams {
    id: number;
}

export interface SearchInstructionsParams {
    category?: InstructionCategory;
    language?: string;
    project?: string;
    tags?: string[];
    query?: string;
    limit?: number;
}
