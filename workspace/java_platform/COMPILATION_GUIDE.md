# Java Platform Compilation Guide

## Overview
This guide documents the correct compilation approach for the Greymatter Platform Java services using Maven's reactor build system.

## Project Structure
The platform consists of multiple microservices with complex interdependencies:
- `wms-common` - Common utilities and shared components
- `wms-notification` - Notification service modules
- `wms-integration` - Integration service modules
- `wms-billing` - Billing service modules
- `wms-storage` - Storage service modules
- `gor-platform-auth` - Authentication service modules
- `validation_engine` - Validation service modules
- `api-gateway` - API Gateway service
- And many more...

## Recommended Compilation Approach

### ⚠️ DO NOT compile modules individually unless absolutely necessary!

Maven's reactor build system automatically resolves and builds dependencies in the correct order. Use the following approach:

### Option 1: Build a Specific Module with Dependencies (RECOMMENDED)
```bash
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -pl <module-name> -am -DskipTests
```

**Flags Explained:**
- `-pl <module-name>` : Specifies the project/module to build
- `-am` : Also make - automatically builds all dependencies
- `-DskipTests` : Skips test execution (faster build)

**Examples:**
```bash
# Build api-gateway and all its dependencies
mvn clean install -pl api-gateway -am -DskipTests

# Build auth service and all its dependencies
mvn clean install -pl gor-platform-auth -am -DskipTests

# Build notification service and all its dependencies
mvn clean install -pl wms-notification -am -DskipTests
```

### Option 2: Build Entire Platform
```bash
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -DskipTests
```
**Note:** This builds all modules and takes longer.

### Option 3: Install Parent POM Only
If you need to install just the parent POM:
```bash
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -N -DskipTests
```
**Flag:** `-N` : Non-recursive (only current project, not modules)

## Actual Dependency Resolution Example

### API Gateway Build (Verified 2025-10-08)

Using the recommended approach:
```bash
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -pl api-gateway -am -DskipTests
```

Maven automatically built dependencies in this order:
1. **parent** (root pom)
2. **wms-common** modules:
   - common-core
   - common-data
   - common-rest-client
3. **wms-notification**:
   - notification-core
   - notification-client
4. **wms-common** (continued):
   - common-kafka
   - transactional-outbox
   - common-rest
5. **wms-integration**:
   - integration-core
   - integration-client
6. **wms-common** (final modules):
   - common-datasource
   - common-redis
7. **validation_engine**:
   - validation-core
   - validation-client
8. **gor-platform-auth**:
   - auth-core
   - auth-client
9. **api-gateway** ✅

**Build Time:** ~19.5 seconds
**Result:** BUILD SUCCESS

## Troubleshooting

### Common Issues

#### 1. Missing Dependencies
**Problem:** Module fails with "Could not find artifact" error

**Solution:**
```bash
# Let Maven's reactor resolve dependencies automatically
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -pl <target-module> -am -DskipTests
```

#### 2. Parent POM Not Found
**Problem:** Build fails with "Could not find artifact com.gor.platform:parent:pom"

**Solution:**
```bash
# Install parent POM first
cd /Users/amar.c/workspace/greymatter-platform
mvn clean install -N -DskipTests
```

#### 3. Circular Dependencies
**Problem:** Build gets stuck or fails due to circular references

**Solution:** Maven's reactor build handles this automatically when using `-am` flag from the root directory.

#### 4. Version Conflicts
**Problem:** Different modules expecting different versions

**Solution:** Check the parent POM's `dependencyManagement` section for version definitions.

### Useful Build Commands

```bash
# Build a specific module with all its dependencies (RECOMMENDED)
mvn clean install -pl <module-name> -am -DskipTests

# Build with tests
mvn clean install -pl <module-name> -am

# Build entire platform
mvn clean install -DskipTests

# Force update dependencies from remote repositories
mvn clean install -pl <module-name> -am -DskipTests -U

# Build only the parent POM
mvn clean install -N -DskipTests

# Resume build from a specific module (after failure)
mvn clean install -rf :<module-name> -DskipTests
```

### Build Performance Tips

1. **Skip Tests:** Use `-DskipTests` for faster builds during development
2. **Parallel Builds:** Use `-T 1C` to build with 1 thread per CPU core
   ```bash
   mvn clean install -pl api-gateway -am -DskipTests -T 1C
   ```
3. **Offline Mode:** Use `-o` if you have all dependencies cached locally
   ```bash
   mvn clean install -pl api-gateway -am -DskipTests -o
   ```

## Module-Specific Build Times (Reference)

| Module | Build Time | Dependencies Built |
|--------|------------|-------------------|
| `api-gateway` | ~19.5s | 23 modules |
| `gor-platform-auth` | ~15s | 21 modules |
| `wms-notification` | ~8s | 6 modules |
| `wms-common` | ~5s | 14 modules |

*Times are approximate with `-DskipTests` flag*

## Key Takeaways

✅ **DO:**
- Always use `-pl <module> -am` from the root directory
- Let Maven's reactor handle dependency order
- Use `-DskipTests` for faster development builds

❌ **DON'T:**
- Manually compile modules in a specific order
- Build individual modules from their directories
- Modify dependencies without understanding the impact

## Notes

- Maven's reactor build is intelligent and handles complex dependency chains
- The `-am` (also-make) flag is crucial for building all required dependencies
- Build times may vary based on system performance and cached dependencies
- Always build from the root project directory when using `-pl` and `-am` flags

---
*Last Updated: 2025-10-08*
*Platform: Greymatter Platform*
*Java Version: 21*
*Maven Version: 3.x*
*Build System: Maven Reactor*
