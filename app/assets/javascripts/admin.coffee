# Since the SpectraViewer can take a while to load,
# use $(window).load rather than $(document).ready, so images are loaded first
$(window).load ->


  $('.results-admin-table').DataTable({
    'paging': false,
    'order': [[ 0, 'asc' ]]
  })







