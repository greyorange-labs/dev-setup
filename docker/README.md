# Docker Compose Monitoring Stack

This Docker Compose setup provides a complete monitoring and messaging stack with the following services:

## Services

### 🚀 Kafka Ecosystem
- **Kafka** (Port 9092): Apache Kafka in KRaft mode (no Zookeeper needed)
- **Kowl** (Port 8080): Modern Kafka Web UI (Redpanda Console)

### 🗄️ Databases
- **PostgreSQL** (Port 5432): Relational database with custom init.sql
- **InfluxDB** (Port 8086): Time-series database for metrics

### 📊 Monitoring & Visualization
- **Telegraf**: Metrics collection agent
- **Grafana** (Port 3000): Analytics and monitoring platform

## Getting Started

### 1. Prerequisites
- Docker and Docker Compose installed
- At least 4GB of available RAM

### 2. Start the Stack
```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop all services
docker-compose down

# Stop and remove volumes (⚠️ this will delete all data)
docker-compose down -v
```

### 3. Access the Services

| Service | URL | Default Credentials |
|---------|-----|-------------------|
| Grafana | http://localhost:3000 | admin / admin123 |
| Kowl (Kafka UI) | http://localhost:8080 | No authentication |
| InfluxDB | http://localhost:8086 | admin / admin123456 |
| PostgreSQL | localhost:5432 | postgres / postgres123 |

### 4. Configuration

#### Environment Variables
Copy and modify the `.env` file to customize credentials and settings:
```bash
cp .env .env.local
# Edit .env.local with your preferred values
```

#### PostgreSQL Initialization
Edit `postgres/init.sql` to customize your database schema, tables, and initial data.

#### Telegraf Configuration
Modify `telegraf/telegraf.conf` to add custom metrics collection or change output destinations.

#### Grafana Datasources
Grafana is pre-configured with:
- InfluxDB datasource (for Telegraf metrics)
- PostgreSQL datasource (for application data)

## Health Checks

Check if all services are running:
```bash
docker-compose ps
```

Test connectivity:
```bash
# Test Kafka (KRaft mode)
docker-compose exec kafka /opt/kafka/bin/kafka-topics.sh --bootstrap-server localhost:9092 --list

# Test PostgreSQL
docker-compose exec postgres psql -U postgres -d monitoring -c "SELECT version();"

# Test InfluxDB
curl -I http://localhost:8086/health
```

## Data Persistence

All data is persisted in Docker volumes:
- `postgres-data`: PostgreSQL database files
- `influxdb-data`: InfluxDB database files
- `grafana-data`: Grafana dashboards and settings
- `kafka-data`: Kafka logs and topics (KRaft metadata included)

## Networking

All services run on a custom bridge network (`amar-dev-net`) allowing secure internal communication.

## Apache Kafka KRaft Mode

This setup uses Apache Kafka in KRaft mode, which offers several advantages:

### ✅ **Benefits:**
- **No Zookeeper Required**: Simplified architecture with built-in coordination
- **Better Performance**: Reduced latency and improved throughput
- **Easier Management**: Single service to monitor and maintain
- **Future-Proof**: KRaft is the future of Kafka (Zookeeper being deprecated)
- **Resource Efficient**: Lower memory footprint and fewer moving parts

### 🔧 **KRaft-Specific Commands:**
```bash
# View Kafka cluster metadata
docker-compose exec kafka /opt/kafka/bin/kafka-metadata-shell.sh --snapshot /opt/kafka/logs/__cluster_metadata-0/00000000000000000000.log

# Check broker registration
docker-compose exec kafka /opt/kafka/bin/kafka-broker-api-versions.sh --bootstrap-server localhost:9092
```

## Resource Requirements

Recommended minimum resources:
- **CPU**: 2 cores
- **RAM**: 3GB (reduced due to no Zookeeper)
- **Disk**: 10GB free space

## Troubleshooting

### Common Issues

1. **Port conflicts**: Modify port mappings in docker-compose.yml
2. **Memory issues**: Increase Docker memory limit or reduce service resource allocation
3. **Startup order**: Services have proper dependency configuration, but you can restart individual services if needed

### Useful Commands
```bash
# Restart a specific service
docker-compose restart grafana

# View service logs
docker-compose logs grafana

# Execute commands in containers
docker-compose exec postgres psql -U postgres
docker-compose exec kafka /opt/kafka/bin/kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic test

# Create Kafka topics
docker-compose exec kafka /opt/kafka/bin/kafka-topics.sh --bootstrap-server localhost:9092 --create --topic my-topic --partitions 3 --replication-factor 1
```

## Next Steps

1. **Configure Grafana Dashboards**: Import or create dashboards for your metrics
2. **Set Up Kafka Topics**: Create topics for your application data streams
3. **Customize Telegraf**: Add application-specific metrics collection
4. **Database Schema**: Extend the PostgreSQL init.sql with your application tables
5. **Monitoring Alerts**: Configure Grafana alerts for critical metrics

## Security Notes

- Change default passwords in production environments
- Consider enabling SSL/TLS for external access
- Restrict network access using proper firewall rules
- Use secrets management for sensitive credentials

For production deployments, review security best practices for each service.