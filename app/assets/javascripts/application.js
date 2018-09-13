// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or vendor/assets/javascripts of plugins, if any, can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file.
//
// Read Sprockets README (https://github.com/sstephenson/sprockets#sprockets-directives) for details
// about supported directives.
//

//= require jquery
//= require jquery.turbolinks
//= require jquery_ujs
// require dataTables/jquery.dataTables
// require dataTables/bootstrap/3/jquery.dataTables.bootstrap
// require dataTables/jquery.dataTables.bootstrap3
//= require wishart
// Must load spectra before ms_dialog
//= require spectra
//= require ms_dialog
// Must load preprocessing before alkane_dialog
//= require preprocessing
//= require_tree .
// require turbolinks  #turbolinks which does not trigger Document.ready event. https://stackoverflow.com/questions/18956989/rails-assets-not-working-properly-on-link-click-but-working-when-refreshed-or
