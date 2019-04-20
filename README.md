## azure-exporter

[![Build status](https://ci.appveyor.com/api/projects/status/36fhpymbr1wma88r/branch/master?svg=true)](https://ci.appveyor.com/project/mujx/azure-exporter/branch/master)

Export metrics from Azure in the Prometheus exposition format.

### Running

#### Docker

```bash
docker run \
   -e YOUR_CLIENT_SECRET_VAR=<secret> \
   -p 3000:3000 \
   -v $(pwd)/config.yaml:/etc/config.yaml \
   mujx/azure-exporter:latest -f /etc/config.yaml
```

#### From source

```bash
stack setup
stack build
stack install

# Add ${HOME}/.local/bin to your PATH; this is where the executable will be installed.
export PATH=${HOME}/.local/bin:$PATH

YOUR_CLIENT_SECRET_VAR=<secret> azure-exporter -f config.yaml
```

Query the metrics endpoint on port `3000`.

```
curl http://localhost:3000/metrics
```

### Options

```
$ azure-exporter --help
Azure Exporter :: v0.1.0.0

Usage: azure-exporter [-f|--config-file FILE]
  Web service that retrieves metrics from Azure and exports them for Prometheus.

Available options:
  -f,--config-file FILE    Exporter settings (default: "config.yaml")
  -h,--help                Show this help text
```

### Configuration

```yaml
---
- resources:
  - name: /resourceGroups/my-resource-group/providers/Microsoft.DBforPostgreSQL/servers/my-db-01
    # All the aggregations will be available for each metric (Total, Maximum, Minimum, Average).
    metrics:
      - active_connections
      - cpu_percent
      - network_bytes_egress
      - network_bytes_ingress
  - name: /resourceGroups/my-resource-group/providers/Microsoft.DBforPostgreSQL/servers/my-db-02
    metrics:
      - active_connections
      - cpu_percent
      - pg_replica_log_delay_in_bytes
      - pg_replica_log_delay_in_seconds
  # Retrieve the Subscription ID from the Azure portal.
  # https://docs.bitnami.com/azure/faq/administration/find-subscription-id/
  subscriptionId: <subscriptionId>
  # Retrieve the Tenant ID (aka Directory ID) from Active Directory.
  # https://stackoverflow.com/questions/26384034/how-to-get-the-azure-account-tenant-id
  tenantId: <tenantId>
  # In order to access the metrics API in Azure, you'll need to register an Application
  # in Active Directory and give it permission to Azure Monitor.
  # Use the ApplicationID as clientId & create a key and use it as the value of
  # the environment variable in clientSecretEnvVar.
  # https://docs.microsoft.com/en-us/azure/active-directory/develop/howto-create-service-principal-portal
  clientId: <clientId>
  # This represents and environment variable that holds the client secret
  # and it should be provided at runtime.
  clientSecretEnvVar: AZURE_CLIENT_SECRET

# You can retrieve metrics from multiple Azure subscriptions (e.g different environments).
- resources:
  - name: /resourceGroups/my-resource-group-02/providers/Microsoft.DBforPostgreSQL/servers/my-db-01
    metrics:
      - active_connections
      - cpu_percent
  subscriptionId: <secondSubscriptionId>
  tenantId: <secondTenantId>
  clientId: <secondClientId>
  clientSecretEnvVar: AZURE_CLIENT_SECRET_2
```
