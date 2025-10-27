import { z } from 'zod';

// MCP Resource schemas
export const InstructionResourceSchema = z.object({
    uri: z.string(),
    name: z.string(),
    description: z.string(),
    mimeType: z.string()
});

export type InstructionResource = z.infer<typeof InstructionResourceSchema>;

// Database schemas
export const InstructionSchema = z.object({
    id: z.number().optional(),
    name: z.string(),
    content: z.string(),
    category: z.string(),
    description: z.string().optional(),
    created_at: z.string().optional(),
    updated_at: z.string().optional()
});

export type Instruction = z.infer<typeof InstructionSchema>;

// MCP Tool schemas
export const SearchInstructionsArgsSchema = z.object({
    query: z.string().describe('Search query for instructions'),
    category: z.string().optional().describe('Optional category filter')
});

export const AddInstructionArgsSchema = z.object({
    name: z.string().describe('Name of the instruction'),
    content: z.string().describe('Content of the instruction'),
    category: z.string().describe('Category of the instruction'),
    description: z.string().optional().describe('Optional description')
});

export const UpdateInstructionArgsSchema = z.object({
    id: z.number().describe('ID of the instruction to update'),
    name: z.string().optional().describe('Updated name'),
    content: z.string().optional().describe('Updated content'),
    category: z.string().optional().describe('Updated category'),
    description: z.string().optional().describe('Updated description')
});

export type SearchInstructionsArgs = z.infer<typeof SearchInstructionsArgsSchema>;
export type AddInstructionArgs = z.infer<typeof AddInstructionArgsSchema>;
export type UpdateInstructionArgs = z.infer<typeof UpdateInstructionArgsSchema>;

// Error types
export class InstructionNotFoundError extends Error {
    constructor(identifier: string | number) {
        super(`Instruction not found: ${identifier}`);
        this.name = 'InstructionNotFoundError';
    }
}

export class DatabaseError extends Error {
    constructor(message: string, public cause?: Error) {
        super(message);
        this.name = 'DatabaseError';
    }
}

export class FileNotFoundError extends Error {
    constructor(filePath: string) {
        super(`File not found: ${filePath}`);
        this.name = 'FileNotFoundError';
    }
}