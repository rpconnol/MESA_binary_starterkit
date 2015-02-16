! ***********************************************************************
!
!   Copyright (C) 2010  Bill Paxton
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************
 
      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use crlibm_lib, only: safe_log10_cr
      
      implicit none
      
      
      logical :: my_win_flag, my_file_flag, my_xaxis_reversed
      integer :: my_file_cnt
      character (len=256) :: my_file_dir, my_file_prefix
      real :: &
         my_win_width, my_win_aspect_ratio, &
         my_file_width, my_file_aspect_ratio
         
      character (len=256) :: my_xaxis, my_yaxis
      real :: &
         my_xmin, my_xmax, &
         my_ymin_left, my_ymax_left, my_dymin_left, &
         my_ymin_right, my_ymax_right, my_dymin_right
      
         
      namelist /my_pgstar/ &
         my_win_flag, my_file_flag, my_xaxis_reversed, &
         my_file_cnt, &
         my_file_dir, my_file_prefix, &
         my_win_width, my_win_aspect_ratio, &
         my_file_width, my_file_aspect_ratio, &
         my_xaxis, my_xmin, my_xmax, &
         my_yaxis, my_ymin_left, my_ymax_left, my_dymin_left, &
         my_ymin_right, my_ymax_right, my_dymin_right
      
      logical :: have_data = .false.
      real, dimension(:), pointer :: my_xdata, my_ydata
      integer :: my_num_points
      

      contains

      
      
      subroutine my_pgstar_plots_info(id, ierr)
         use utils_lib, only: alloc_iounit, free_iounit
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         
         integer, parameter :: num_Other_plots = 1 ! can have up to max_num_Other_plots
         integer :: i, plot_id, iounit
         type (pgstar_win_file_data), pointer :: p
         type (star_info), pointer :: s
         character(len=64) :: fname

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         call set_my_namelist_defaults
         call read_my_pgstar_namelist('inlist_my_pgstar', ierr)
         if (ierr /= 0) return
         
         do i = 1, num_Other_plots
            plot_id = i_Other + i - 1
            p => s% pgstar_win_file_ptr(plot_id)
            p% plot => my_plot
            p% id = plot_id
            p% name = 'My_Plot'
            p% win_flag = my_win_flag
            p% win_width = my_win_width
            p% win_aspect_ratio = my_win_aspect_ratio
            p% file_flag = my_file_flag
            p% file_dir = my_file_dir
            p% file_prefix = my_file_prefix
            p% file_cnt = my_file_cnt
            p% file_width = my_file_width
            p% file_aspect_ratio = my_file_aspect_ratio
         end do
         
         if (have_data) return
         ! read the data
         fname = 'entropy_profile.txt'
         iounit = alloc_iounit(ierr); if (ierr /= 0) return
         open(iounit, file=trim(fname), action='read', status='old', iostat=ierr)
         if (ierr /= 0) then
            write(*, *) 'failed to open ' // trim(fname)
            call free_iounit(iounit)
            return
         end if                  
         read(iounit, *, iostat=ierr) my_num_points
         if (ierr /= 0) then
            write(*, *) 'failed to read num points on 1st line ' // trim(fname)
            call free_iounit(iounit)
            return
         end if
         allocate(my_xdata(my_num_points), my_ydata(my_num_points))
         do i=1,my_num_points
            read(iounit, *, iostat=ierr) my_xdata(i), my_ydata(i)
            if (ierr /= 0) then
               write(*, *) 'failed to read data ' // trim(fname)
               call free_iounit(iounit)
               return
            end if
         end do
         close(iounit)
         call free_iounit(iounit)
         have_data = .true.
         
      end subroutine my_pgstar_plots_info
      
      
      subroutine set_my_namelist_defaults
      
         my_win_flag = .false.

         my_win_width = 7
         my_win_aspect_ratio = 0.62 ! aspect_ratio = height/width
         
         my_xaxis = 'mass'
         my_xaxis_reversed = .false.
         my_xmin = -101 ! only used if > -100
         my_xmax = -101 ! only used if > -100
         
         my_yaxis = 'entropy'
         my_ymin_left = -101 ! only used if > -100
         my_ymax_left = -101 ! only used if > -100        
         my_dymin_left = -101 ! only used if > -100
         
         my_ymin_right = -101 ! only used if > -100
         my_ymax_right = -101 ! only used if > -100        
         my_dymin_right = -101 ! only used if > -100 
         
         ! file output
         my_file_flag = .false.
         my_file_dir = 'pgstar_out'
         my_file_prefix = 'profile'
         my_file_cnt = 5 ! output when mod(model_number,my_file_cnt)==0
         my_file_width = -1 ! negative means use same value as for window
         my_file_aspect_ratio = -1 ! negative means use same value as for window
         
      end subroutine set_my_namelist_defaults
      
      
      subroutine read_my_pgstar_namelist(filename, ierr)
         use utils_lib
         character(*), intent(in) :: filename
         integer, intent(out) :: ierr

         integer :: unit 
         
         ierr = 0
         unit=alloc_iounit(ierr)
         if (ierr /= 0) return
         
         open(unit=unit, file=trim(filename), action='read', delim='quote', status='old', iostat=ierr)
         if (ierr /= 0) then
            write(*,'(a)') 'Failed to open control namelist file '// trim(filename)
            return
         end if
         read(unit, nml=my_pgstar, iostat=ierr)  
         close(unit)
         
         if (ierr /= 0) then
            write(*, *) 
            write(*, *) 
            write(*, *) 
            write(*, *) 
            write(*, '(a)') &
               'Failed while trying to read control namelist file: ' // trim(filename)
            write(*, '(a)') &
               'Perhaps the following runtime error message will help you find the problem.'
            write(*, *) 
            open(unit=unit, file=trim(filename), action='read', delim='quote', status='old', iostat=ierr)
            read(unit, nml=my_pgstar)
            close(unit)
            call free_iounit(unit)
            return
         end if
         
         call free_iounit(unit)
      
      end subroutine read_my_pgstar_namelist
      

      

      subroutine my_plot(id, device_id, ierr)
         integer, intent(in) :: id, device_id
         integer, intent(out) :: ierr
         
         real :: winxmin, winxmax, winymin, winymax, label_scale

         type (star_info), pointer :: s

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         call pgslct(device_id)
         call pgbbuf()
         call pgeras()

         winxmin = 0.14
         winxmax = 0.85
         winymin = 0.13
         winymax = 0.92
         label_scale = 1.2
         
         call do_my_plot(s, device_id, &
            winxmin, winxmax, winymin, winymax, &
            label_scale, ierr)

         call pgebuf()
      
      end subroutine my_plot


      subroutine do_my_plot(s, device_id, &
            winxmin, winxmax, winymin, winymax, label_scale, ierr)
            
         use utils_lib
         use const_def

         type (star_info), pointer :: s
         integer, intent(in) :: device_id
         real, intent(in) :: winxmin, winxmax, winymin, winymax, label_scale
         integer, intent(out) :: ierr
         
         real :: windy, xmargin
         real :: xmin, xmax, xleft, xright, dx, tmp, ymin, ymax, dy
         integer :: grid_min, grid_max, npts, nz
         real, pointer, dimension(:) :: xvec, yvec
         
         logical :: dbg = .false.
         
         include 'formats.inc'
         ierr = 0
         xmargin = 0

         nz = s% nz
         allocate (xvec(nz), yvec(nz))
         
         call set_pgstar_xaxis_bounds( &
            s, my_xaxis, my_xmin, my_xmax, xmargin, &
            xvec, xmin, xmax, xleft, xright, dx, &
            grid_min, grid_max, npts, ierr)
         if (ierr /= 0) return
      
         if (my_xaxis_reversed) then
            xright = xmin; xleft = xmax
         else
            xright = xmax; xleft = xmin
         end if
         
         if (dbg) then
            write(*,1) 'my_xaxis ' // trim(my_xaxis)
            write(*,1) 'my_xmin', my_xmin
            write(*,1) 'my_xmax', my_xmax
            write(*,2) 'grid_min', grid_min
            write(*,2) 'grid_max', grid_max
            write(*,1) 'xmin', xmin
            write(*,1) 'xmax', xmax
            write(*,1) 'xleft', xleft
            write(*,1) 'xright', xright
            write(*,1) 'dx', dx
         end if

         call plot(ierr)
         if (ierr /= 0) return

         deallocate(xvec, yvec)
         
         
         contains
         
         
         subroutine plot(ierr) 
           use crlibm_lib, only: safe_log10_cr 
           use rates_def, only: i_rate 
           use chem_def, only: ipp, icno 
           integer, intent(out) :: ierr 

           integer :: lw, lw_sav, k 
           real :: ybot, eps, & 
           default_ymax, default_ymin 
           character (len=128) :: str 

           include 'formats.inc' 
           ierr = 0 

           call pgsave 

           lw = 6 
           call pgqlw(lw_sav) 

           call pgsvp(winxmin, winxmax, winymin, winymax) 

           ! title 
           call pgstar_show_title(s, 'Entropy', 0.0) 

           call pgsch(label_scale) 
           write(str,'(i9)') s% model_number 
           call pgmtxt('T',1.8,0.9,0.5,str) 

           ! xlabel 
           call pgsci(1) 
           call pgsch(label_scale) 
           call show_pgstar_xaxis_by(s,my_xaxis,ierr) 
           if (ierr /= 0) return 

           default_ymax = 10 
           default_ymin = -2 

           yvec = s% entropy(1:nz) 

           if (my_ymax_right > -100) then
             ymax = my_ymax_right
           else
             ymax = max(default_ymax,maxval(yvec(grid_min:grid_max))) 
           end if

           if (my_ymin_right > -100) then
             ymin = my_ymin_right
           else
             ymin = max(default_ymin,minval(yvec(grid_min:grid_max))) 
           end if

           dy = ymax-ymin 
           if (dy == 0) dy = 1 
!            if (my_dymin > -100) dy = my_dymin

           ymax = ymax + 0.1*dy 
           ymin = ymin - 0.1*dy 

           if (dbg) then 
           write(*,1) 'left axis xleft, xright', xleft, xright 
           write(*,1) 'left axis ymin, ymax, dy', ymin, ymax, dy 
           end if 

           call pgswin(xleft, xright, ymin, ymax) 
           call pgscf(1) 
           call pgsci(1) 
           call pgsch(label_scale) 
           call pgbox('BCNST',0.0,0,'BCNSTMV',0.0,0) 

           call pgsci(clr_Teal) 
           call pgsch(label_scale) 
           call pgstar_show_left_yaxis_label(s, 'model entropy', 0.0) 

           call pgslw(lw) 
           call pgline(npts, xvec(grid_min:grid_max), yvec(grid_min:grid_max)) 
           call pgslw(lw_sav) 

           call pgsci(clr_Coral) 
           call pgstar_show_right_yaxis_label(s, 'base entropy', 0.0) 

           call pgslw(lw) 
           call pgline(my_num_points, my_xdata, my_ydata) 
           call pgslw(lw_sav) 
           call pgunsa 

           end subroutine plot        
         
      end subroutine do_my_plot

      
      subroutine extras_controls(s, ierr)
         type (star_info), pointer :: s
         integer, intent(out) :: ierr
         ierr = 0
         s% other_pgstar_plots_info => my_pgstar_plots_info
         s% use_other_pgstar_plots = .true.
      end subroutine extras_controls
      
      
      integer function extras_startup(s, id, restart, ierr)
         type (star_info), pointer :: s
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         ierr = 0
         extras_startup = 0
         if (.not. restart) then
            call alloc_extra_info(s)
         else ! it is a restart
            call unpack_extra_info(s)
         end if
      end function extras_startup
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(s, id, id_extra)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra
         extras_check_model = keep_going         
         if (.false. .and. s% star_mass_h1 < 0.35d0) then
            ! stop when star hydrogen mass drops to specified level
            extras_check_model = terminate
            write(*, *) 'have reached desired hydrogen mass'
            return
         end if


         ! if you want to check multiple conditions, it can be useful
         ! to set a different termination code depending on which
         ! condition was triggered.  MESA provides 9 customizeable
         ! termination codes, named t_xtra1 .. t_xtra9.  You can
         ! customize the messages that will be printed upon exit by
         ! setting the corresponding termination_code_str value.
         ! termination_code_str(t_xtra1) = 'my termination condition'

         ! by default, indicate where (in the code) MESA terminated
         if (extras_check_model == terminate) s% termination_code = t_extras_check_model
      end function extras_check_model


      integer function how_many_extra_history_columns(s, id, id_extra)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra
         how_many_extra_history_columns = 1
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(s, id, id_extra, n, names, vals, ierr)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         
         !note: do NOT add these names to history_columns.list
         ! the history_columns.list is only for the built-in log column options.
         ! it must not include the new column names you are adding here.
         
         names(1) = "log_star_mass"
         vals(1) = safe_log10_cr(s% star_mass)
         ierr = 0
      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(s, id, id_extra)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(s, id, id_extra, n, nz, names, vals, ierr)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         integer :: k
         ierr = 0
         
         !note: do NOT add these names to profile_columns.list
         ! the profile_columns.list is only for the built-in profile column options.
         ! it must not include the new column names you are adding here.

         ! here is an example for adding a profile column
         !if (n /= 1) stop 'data_for_extra_profile_columns'
         !names(1) = 'beta'
         !do k = 1, nz
         !   vals(k,1) = s% Pgas(k)/s% P(k)
         !end do
         
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going or terminate.
      ! note: cannot request retry or backup; extras_check_model can do that.
      integer function extras_finish_step(s, id, id_extra)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra
         integer :: ierr
         extras_finish_step = keep_going
         call store_extra_info(s)

         ! to save a profile, 
            ! s% need_to_save_profiles_now = .true.
         ! to update the star log,
            ! s% need_to_update_history_now = .true.

         ! see extras_check_model for information about custom termination codes
         ! by default, indicate where (in the code) MESA terminated
         if (extras_finish_step == terminate) s% termination_code = t_extras_finish_step
      end function extras_finish_step
      
      
      subroutine extras_after_evolve(s, id, id_extra, ierr)
         type (star_info), pointer :: s
         integer, intent(in) :: id, id_extra
         integer, intent(out) :: ierr
         ierr = 0
      end subroutine extras_after_evolve
      
      
      ! routines for saving and restoring extra data so can do restarts
         
         ! put these defs at the top and delete from the following routines
         !integer, parameter :: extra_info_alloc = 1
         !integer, parameter :: extra_info_get = 2
         !integer, parameter :: extra_info_put = 3
      
      
      subroutine alloc_extra_info(s)
         integer, parameter :: extra_info_alloc = 1
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_alloc)
      end subroutine alloc_extra_info
      
      
      subroutine unpack_extra_info(s)
         integer, parameter :: extra_info_get = 2
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_get)
      end subroutine unpack_extra_info
      
      
      subroutine store_extra_info(s)
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_put)
      end subroutine store_extra_info
      
      
      subroutine move_extra_info(s,op)
         integer, parameter :: extra_info_alloc = 1
         integer, parameter :: extra_info_get = 2
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         integer, intent(in) :: op
         
         integer :: i, j, num_ints, num_dbls, ierr
         
         i = 0
         ! call move_int or move_flg    
         num_ints = i
         
         i = 0
         ! call move_dbl       
         
         num_dbls = i
         
         if (op /= extra_info_alloc) return
         if (num_ints == 0 .and. num_dbls == 0) return
         
         ierr = 0
         call star_alloc_extras(s% id, num_ints, num_dbls, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in star_alloc_extras'
            write(*,*) 'alloc_extras num_ints', num_ints
            write(*,*) 'alloc_extras num_dbls', num_dbls
            stop 1
         end if
         
         contains
         
         subroutine move_dbl(dbl)
            real(dp) :: dbl
            i = i+1
            select case (op)
            case (extra_info_get)
               dbl = s% extra_work(i)
            case (extra_info_put)
               s% extra_work(i) = dbl
            end select
         end subroutine move_dbl
         
         subroutine move_int(int)
            integer :: int
            i = i+1
            select case (op)
            case (extra_info_get)
               int = s% extra_iwork(i)
            case (extra_info_put)
               s% extra_iwork(i) = int
            end select
         end subroutine move_int
         
         subroutine move_flg(flg)
            logical :: flg
            i = i+1
            select case (op)
            case (extra_info_get)
               flg = (s% extra_iwork(i) /= 0)
            case (extra_info_put)
               if (flg) then
                  s% extra_iwork(i) = 1
               else
                  s% extra_iwork(i) = 0
               end if
            end select
         end subroutine move_flg
      
      end subroutine move_extra_info

      end module run_star_extras
      
