FROM ubuntu:20.04

RUN apt update
RUN apt -y upgrade
RUN apt -y install gcc g++ nodejs autoconf bison build-essential libssl-dev libyaml-dev libreadline-dev zlib1g-dev libncurses5-dev libffi-dev libgdbm6 libgdbm-dev git libmysqlclient-dev
RUN apt -y install libnetcdf-dev libnetcdff-dev

RUN apt -y install libcairo2-dev # for Cario library in R
RUN apt -y install libxt-dev
RUN apt -y install libxml2-dev libxml2


##### Install ruby and rails ######
RUN git clone https://github.com/rbenv/rbenv.git ~/.rbenv
RUN echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
RUN echo 'eval "$(rbenv init -)"' >> ~/.bashrc
RUN source ~/.bashrc

RUN git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
RUN rbenv install 2.7.1
RUN rbenv global 2.7.1
RUN echo "gem: --no-document" > ~/.gemrc

#####  Test git to bitbucket ######
RUN mkdir -p /root/.ssh/ && chmod 600 -R /root/.ssh
ADD id_rsa  /root/.ssh/id_rsa
RUN chmod 600 /root/.ssh/id_rsa
RUN ssh-keyscan -t rsa bitbucket.org > ~/.ssh/known_hosts
RUN ssh -T git@bitbucket.org


# Install R 3.4
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt update
RUN apt install -y r-base

# Instal R lib
COPY install_r_dependencies.R /root/.
RUN  Rscript /root/install_R_dependancies.R


# Install rails app
RUN git clone git@bitbucket.org:wishartlab/gc-autofit.git --branch rails-5 --single-branch /root/gc-autofit
RUN gem install bundler

WORKDIR "/root/gc-autofit"

RUN bundle install


# Setup rails app
RUN bundle exec rake db:create RAILS_ENV=production
RUN bundle exec rake db:migrate RAILS_ENV=production

COPY database.yml config/database.yml
COPY sidekiq.rb   config/initializers/sidekiq.rb  # this define which redis port sidekiq will connect

# https://stackoverflow.com/questions/22958188/how-to-run-sidekiq-in-production-server 
# run at demon
RUN bundle exec sidekiq -d -L log/sidekiq.log -C config/sidekiq.yml -e production

CMD [ "bundle", "exec", "rails", "s", "-p", "3003", "-b", "0.0.0.0"]





