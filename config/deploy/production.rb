set :stage, :production

# Simple Role Syntax
# ==================
# Supports bulk-adding hosts to roles, the primary
# server in each group is considered to be the first
# unless any hosts have the primary property set.
# role :app, %w{deploy@example.com}
# role :web, %w{deploy@example.com}
# role :db,  %w{deploy@example.com}

# Extended Server Syntax
# ======================
# This can be used to drop a more detailed server
# definition into the server list. The second argument
# something that quacks like a hash can be used to set
# extended properties on the server.
#server '104.154.69.124', user: 'gcms', roles: %w{web app db}
## new instance created from previous a snapshot
#server '104.198.157.103', user: 'gcms', roles: %w{web app db}
server '104.154.43.101', user: 'gcms', roles: %w{web app db}
