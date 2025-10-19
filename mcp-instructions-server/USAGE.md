# Usage Guide - MCP Instructions Server

## 🎓 How to Use with GitHub Copilot

Once the MCP server is configured, GitHub Copilot will automatically have access to your instructions. Here's how to get the most out of it:

## 📋 Example Prompts

### 1. Starting a New Feature

**Instead of:**
> "Add a new user API endpoint"

**Try:**
> "I want to add a new user API endpoint to the GreyMatter Platform. Follow the workflow rules."

**What Copilot Will Do:**
1. ✅ Read `instructions://workflow-rules`
2. ✅ Read `instructions://project-config/greymatter-platform`
3. ✅ Read `instructions://coding-patterns/java`
4. ✅ Clarify requirements (Phase 1)
5. ✅ Create detailed plan (Phase 2)
6. ✅ Implement following Spring Boot patterns (Phase 3)
7. ✅ Validate and summarize (Phase 4)

---

### 2. Working with Erlang/Butler Server

**Prompt:**
> "Add a new gen_server for handling warehouse events in butler_server. Use the proper patterns."

**What Copilot Will Do:**
1. Read Erlang/OTP patterns
2. Read Butler Server project config
3. Follow database migration order rules
4. Use proper supervisor setup
5. Include type specs
6. Avoid atom exhaustion pitfalls

---

### 3. Debugging/Fixing Issues

**Prompt:**
> "I'm getting LazyInitializationException in my Java service. Help me fix it properly."

**What Copilot Will Do:**
1. Read `instructions://pitfalls/java`
2. Identify the anti-pattern
3. Suggest proper transaction boundaries
4. Show correct DTO pattern
5. Provide complete working solution

---

### 4. Code Review

**Prompt:**
> "Review this gen_server code for common Erlang pitfalls."

**What Copilot Will Do:**
1. Read `instructions://pitfalls/erlang`
2. Check for atom exhaustion risks
3. Check for process leaks
4. Check ETS table cleanup
5. Verify timeout usage
6. Suggest improvements

---

## 🎯 Best Practices for Prompts

### ✅ Good Prompts

1. **Be Specific About Project**
   ```
   "Add logging to the Butler Server worker_manager module"
   ```
   → Copilot knows it's Erlang/OTP, reads butler-server config

2. **Mention Workflow Phase**
   ```
   "Create a plan for adding user authentication"
   ```
   → Copilot follows Phase 2: Planning

3. **Ask for Pattern Checks**
   ```
   "Check this code against Java best practices"
   ```
   → Copilot reads java-patterns and pitfalls

4. **Request Context-Aware Help**
   ```
   "What patterns should I follow for this file?"
   ```
   → Copilot uses current file context

### ❌ Avoid These

1. **Too Vague**
   ```
   "Add a function"
   ```
   → No context, won't use instructions effectively

2. **Skipping Phases**
   ```
   "Just implement it quickly"
   ```
   → Might skip clarification and planning

3. **No Project Context**
   ```
   "Fix this code"
   ```
   → Won't know which project patterns to apply

---

## 🔍 Checking Available Instructions

### List What's Available

**Prompt:**
> "What instructions do you have access to?"

Or use tools:
```typescript
// Call list_categories tool
{
  "tool": "list_categories"
}

// Call list_languages tool
{
  "tool": "list_languages"
}

// Call list_projects tool
{
  "tool": "list_projects"
}
```

---

## 🛠️ Adding Custom Instructions

### Via Prompt

**Prompt:**
> "Add a new instruction: When creating API endpoints in Butler Server, always version them under /api/v1/"

Copilot will use the `add_instruction` tool:
```typescript
{
  "title": "API Versioning in Butler Server",
  "content": "When creating API endpoints in Butler Server, always version them under /api/v1/",
  "category": "component-guidelines",
  "project": "butler-server",
  "tags": ["api", "versioning"],
  "priority": 90
}
```

### Via Direct Tool Call

**Prompt:**
> "Use the add_instruction tool to add this pattern..."

Then specify the details.

---

## 📚 Resource Access Patterns

### 1. Automatic Context-Aware

When editing a file in Butler Server:
- Copilot automatically reads `instructions://context-aware`
- Gets Erlang patterns
- Gets butler-server project config
- Gets component-specific guidelines

### 2. Explicit Resource Request

**Prompt:**
> "Show me the workflow rules"

Copilot reads: `instructions://workflow-rules`

**Prompt:**
> "What are the Java Spring Boot patterns?"

Copilot reads: `instructions://coding-patterns/java`

### 3. Pitfall Checks

**Prompt:**
> "Check this for Erlang anti-patterns"

Copilot reads: `instructions://pitfalls/erlang`

---

## 🎬 Complete Workflow Example

### Scenario: Add New Feature to Java Project

#### **Step 1: Initial Request**
```
"I need to add a product inventory API to the GreyMatter Platform"
```

#### **Copilot Response (Phase 1):**
```markdown
### Requirement Analysis

**What I Understand:**
- Add REST API endpoints for product inventory management
- CRUD operations for inventory items
- Part of GreyMatter Platform (Java/Spring Boot)

**Affected Components:**
- New module: product-service
- Database: products table
- REST endpoints: /api/v1/products

**Assumptions:**
- Using PostgreSQL database
- Following Spring Boot REST patterns
- Need DTOs (not exposing entities)
- Using Flyway for migrations

**Questions:**
1. Should this integrate with existing user authentication?
2. What fields should a product have (name, SKU, quantity)?
3. Any specific business rules for inventory management?

**Please confirm before I proceed to planning.**
```

#### **You:**
```
"Yes to authentication. Product needs: id, sku, name, quantity, warehouse_id.
No business rules for now, just basic CRUD."
```

#### **Copilot Response (Phase 2):**
```markdown
### Implementation Plan

**Files to Create:**
1. src/main/resources/db/migration/V001__create_products_table.sql
2. src/main/java/com/example/entity/Product.java
3. src/main/java/com/example/dto/ProductDto.java
4. src/main/java/com/example/dto/CreateProductRequest.java
5. src/main/java/com/example/repository/ProductRepository.java
6. src/main/java/com/example/service/ProductService.java
7. src/main/java/com/example/controller/ProductController.java
8. src/test/java/com/example/service/ProductServiceTest.java

**Order of Implementation:**
1. Create database migration (schema first!)
2. Create entity with proper JPA annotations
3. Create DTOs for request/response
4. Create repository interface
5. Create service layer with @Transactional
6. Create REST controller
7. Write tests

**Testing Strategy:**
- Unit tests: Service layer with Mockito
- Integration tests: Controller with @SpringBootTest
- Repository tests: @DataJpaTest
- Manual: Test via Swagger UI

**Risks & Mitigations:**
- Risk: N+1 queries on warehouse relationship
  → Mitigation: Use @EntityGraph or JOIN FETCH
- Risk: Exposing entity in API
  → Mitigation: Always use DTOs
- Risk: No validation on requests
  → Mitigation: Use @Valid with validation annotations

**Please review and approve before I start implementation.**
```

#### **You:**
```
"Approved, proceed"
```

#### **Copilot Response (Phase 3):**
```markdown
Implementing now...

[Creates all files following Java patterns]
[Uses proper transaction boundaries]
[Validates with DTOs]
[Avoids common pitfalls]
```

#### **Copilot Response (Phase 4):**
```markdown
### Implementation Summary

**Files Changed:**
- V001__create_products_table.sql: Created products table with indexes
- Product.java: JPA entity with proper equals/hashCode
- ProductDto.java: Response DTO
- CreateProductRequest.java: Request DTO with validation
- ProductRepository.java: Spring Data repository
- ProductService.java: Service with @Transactional
- ProductController.java: REST controller with proper error handling
- ProductServiceTest.java: Unit tests

**Validation Results:**
✅ Compilation: No errors
✅ Tests: 8/8 passed
✅ Patterns followed:
   - Used DTOs (not entities) in controller
   - Proper transaction boundaries
   - Validation on requests
   - ResponseEntity for flexible responses

**Next Steps:**
1. Run mvn clean install to verify build
2. Start application and test via Swagger
3. Verify Flyway migration runs
4. Test all CRUD operations manually
5. Update API documentation

**Known Limitations:**
- No pagination yet (add if needed for large datasets)
- No filtering/search (can add query methods later)
```

---

## 🎓 Training Your Team

### For New Developers

1. **Show them the workflow**
   ```
   "Explain the 4-phase workflow for this project"
   ```

2. **Teach patterns**
   ```
   "What are the coding patterns I should follow in Butler Server?"
   ```

3. **Review common mistakes**
   ```
   "What are common Erlang pitfalls to avoid?"
   ```

### For Code Reviews

Use Copilot as a second reviewer:
```
"Review this pull request against our coding standards"
```

---

## 🔄 Iterative Improvements

### Adding New Patterns

As you discover new patterns in your codebase:

```
"Add this as a new pattern: [explain pattern]"
```

### Updating Existing Rules

```
"Update the Butler Server config to include: [new rule]"
```

### Team-Specific Conventions

```
"Add a rule that all API responses must include a request_id for tracing"
```

---

## 💡 Pro Tips

1. **Start Every Feature with Clarification**
   - Let Copilot read the workflow rules
   - Force the 4-phase process
   - Reduce errors dramatically

2. **Reference Instructions Explicitly**
   ```
   "Follow the Java Spring Boot patterns from the instructions"
   ```

3. **Use for Learning**
   ```
   "Explain the gen_server pattern from the Erlang instructions"
   ```

4. **Continuous Improvement**
   - Add new patterns as you discover them
   - Update based on code review feedback
   - Share across team

5. **Context is King**
   - The server auto-detects project context
   - Make sure file paths are correct
   - Test in actual project directories

---

## 🐛 Troubleshooting

### Copilot Not Following Instructions

1. **Explicitly mention it:**
   ```
   "Follow the workflow rules from the MCP instructions"
   ```

2. **Reference specific resource:**
   ```
   "Use the Erlang patterns we have defined"
   ```

3. **Check server is running:**
   - Restart VS Code
   - Check Output panel for MCP logs

### Instructions Not Updating

1. **Rebuild database:**
   ```bash
   rm instructions.db
   npm run seed
   ```

2. **Restart MCP server:**
   - Restart VS Code

### Wrong Project Context

1. **Make sure you're in project directory**
2. **Check file path detection in `src/index.ts`**
3. **Explicitly mention project:**
   ```
   "In the Butler Server project, add..."
   ```

---

Happy Coding! 🚀
