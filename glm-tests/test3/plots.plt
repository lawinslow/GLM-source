!-----------------------------------------------------------
! plots_window defines the size of the window used for
! display of the plots
!
! builtin variables that can be plotted are :
!       "temp"
!       "salt"
!       "rad"
!       "extc"
!       "dens"
!       "uorb"
!       "taub"
!
! other variables will depend on the wq family and models chosen
!
!-----------------------------------------------------------

&plots_window
  width = 1500
  height = 900
/

!-----------------------------------------------------------
! plots
!  nplots - the number of plots
!  title  - list of titles, one for each plot
!  min_z  - list of minimum values for the z component
!  max_z  - list of maximum values for the z component
!
! For the moment really only the min/max values are worth
! tinkering with
!-----------------------------------------------------------
&plots
  nplots = 9
  plot_width = 400
  plot_height = 200
! title_font = 'times:bold:italic'
  title_font = 'times:bold'
  title_size = 12
! label_font = 'helvetica:italic'
  label_font = 'helvetica'
  label_size =  10
! title = "Temperature", "Salinity",'DO','DIC','Ph','CH4','NO3','PO4','GreenIN'
! vars  = 'temp','salt','aed_oxygen_oxy','aed_carbon_dic','aed_carbon_pH','aed_carbon_ch4','aed_nitrogen_nit','aed_phosphorus_frp','aed_phytoplankton_green_IN'
! min_z =  0.0, 0.0,    0.0,    1.0,  0.0,  0.0,   0.,  0.,  0.
! max_z = 27.0, 0.91, 800.0, 1800.0, 20.0, 28.0, 130., 10.,  5.
!
! title = 'Temperature', 'Salinity','DO','Green','Diatom','Crypto','NO3','PO4','GreenIN'
! vars  = 'temp','salt','aed_oxygen_oxy','aed_phytoplankton_green','aed_phytoplankton_diatom','aed_phytoplankton_crypto','aed_nitrogen_nit','aed_phosphorus_frp','aed_phytoplankton_green_IN'
! min_z =  0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0., 0., 0.
! max_z = 27.0, 0.91, 600.0, 150.0, 350.0, 2.5, 100., 5., 5.
 !title = 'Temperature', 'Salinity','DO','Green','Diatom','Crypto','NO3','PO4','TCHLA'
 !vars  = 'temp','salt','OXY_oxy','PHY_green','PHY_diatom','PHY_crypto','NIT_nit','PHS_frp','PHY_TCHLA'
 !min_z =  0.0, 0.00,   0.0,  0.0,  0.0,   0.0,  0., 0.0,  0.
 !max_z = 27.0, 0.91, 400.0, 10.0, 15.0, 150.0, 60., 1.5, 32.

! title = 'Temperature', 'Salinity',  'DO',    'Green',      'Diatom',    'Crypto',    'NO3',    'PO4',    'TCHLA'
! vars  =    'temp',      'salt',   'OXY_oxy', 'PHY_green', 'PHY_diatom', 'PHY_crypto','NIT_nit','PHS_frp', 'PHY_TCHLA'
! min_z =       0.0,        0.0,       0.0,       0.0,        0.0,           0.0,         0.0,      0.0,       0.0
! max_z =      27.0,        0.91,    400.0,      10.0,      200.0,           6.0,        60.0,      1.5,      32.0

  title = 'Temperature', 'Salinity',  'DO',    'Green',      'Diatom',    'Crypto',    'NO3',    'test_pel',    'test_ben'
  vars  =    'temp',      'salt',   'OXY_oxy', 'PHY_green', 'PHY_diatom', 'PHY_crypto','NIT_nit','TST_pel', 'TST_ben'
  min_z =       0.0,        0.0,       0.0,       0.0,        0.0,           0.0,         0.0,      0.0,       0.0
  max_z =      27.0,        0.91,    400.0,      10.0,      200.0,           6.0,        60.0,     25.0,      20.0
/

!-----------------------------------------------------------
&validation
  time =
  depths =
/
