#require 'puma/capistrano'


set :application, 'gc-autofit'
set :repo_url,  "git@bitbucket.org:wishartlab/gc-autofit.git"
set :branch, 'master'
set :scm, :git
set :deploy_to, '/apps/gcms/project'
set :use_sudo, false
set :linked_files, %w{config/database.yml}
set :linked_dirs, %w{public/system log APGCMS_working}

# set :default_env, { path: "/opt/ruby/bin:$PATH" }
set :keep_releases, 5
set :sidekiq_config, "#{current_path}/config/sidekiq.yml"
set :sidekiq_pid,  File.join('/', 'tmp', 'gc-autofit.sidekiq.pid')

namespace :deploy do

  desc 'Start application'
  task :start do
    on roles(:web) do
      within release_path do
        execute "script/puma.sh", "start"
      end
    end
  end

  desc 'Stop application'
  task :stop do
    on roles(:web) do
      within release_path do
        execute "script/puma.sh", "stop"
      end
    end
  end

  desc 'Restart application'
  task :restart do
    on roles(:web) do
      # Your restart mechanism here, for example:
      # execute :touch, release_path.join('tmp/restart.txt')
      within release_path do
        execute "script/puma.sh", "restart"
      end
    end
  end

  after :restart, :clear_cache do
    on roles(:web) do
      # Here we can do anything such as:
      # within release_path do
      #   execute :rake, 'cache:clear'
      # end
    end
  end

  after :finishing, 'deploy:cleanup'
  after :finished, :restart

end
