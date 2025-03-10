! ***********************************************************************
!
!   This file is part of a mesa extension.
!   Authors of this file: Tassos Fragos, Jeff J. Andrews, Matthias U. Kruckow
!
! ***********************************************************************
!
!   Copyright (C) 2010-2019  Bill Paxton & The MESA Team
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

      module CE_SMS_accretion

      ! NOTE: if you'd like to have some inlist controls for your routine,
      ! you can use the x_ctrl array of real(dp) variables that is in &controls
      ! e.g., in the &controls inlist, you can set
      !     x_ctrl(1) = <my_special_param>
      ! where <my_special_param> is a real value such as 0d0 or 3.59d0
      ! Then in your routine, you can access that by
      !     s% x_ctrl(1)
      ! of course before you can use s, you need to get it using the id argument.
      ! here's an example of how to do that -- add these lines at the start of your routine:
      !         use star_lib, only: star_ptr
      !         type (star_info), pointer :: s
      !         call star_ptr(id, s, ierr)
      !         if (ierr /= 0) then ! OOPS
      !            return
      !         end if
      !
      ! for integer control values, you can use x_integer_ctrl
      ! for logical control values, you can use x_logical_ctrl

      use star_def
      use const_def
      !use CE_orbit, only: AtoP, TukeyWindow, calc_quantities_at_comp_position

      implicit none

      contains

! ***********************************************************************
      !Function Accretion of mass as in Haemmerlé et al.(2016)
          subroutine SMS_adjust_mdot(id, ierr) 
            use star_def
            integer, intent(in) :: id
            integer, intent(out) :: ierr
            real(dp) :: f, w, log_mdot_out, mdot_out, m2r
    
            type (star_info), pointer :: s
    
            ierr = 0
            call star_ptr(id, s, ierr)
            if (ierr /= 0) then
              write(*,*) 'failed in star_ptr'
               return
            end if
         
            s% mstar_dot = 0.0d0
            w = 0.0d0
            s% explicit_mstar_dot = s% mstar_dot
            ! Mass to reach 
            m2r = 2.0d4
            if (s% x_logical_ctrl(4)) then
              if (s% star_mass <= 5.0d0) then
              
                f = 1.0/3.0
                
              else if (s% star_mass > 5.0 .and. s% star_mass <= m2r) then
              
                f = 1.0/11.0
              end if

              log_mdot_out = -5.28 + s% log_surface_luminosity *(0.752 - 0.0278*s% log_surface_luminosity) ![M_sun/yr]
              mdot_out = 10**(log_mdot_out) * (Msun/secyer) * 10  ![gr/s]  10 for high accretion
    
              w = f/(1-f)*mdot_out 
                 
            endif
            
            if (s% star_mass <= m2r ) then
              s% mstar_dot = (s% mstar_dot + w)
              s% explicit_mstar_dot = s% mstar_dot 
              !write(*,*) 'M<1e4  mstar_dot= ', s% mstar_dot
             
            
            else if(s% star_mass > m2r) then 
              s% mstar_dot = 0.0d0
              s% x_logical_ctrl(4) = .false.
              s% use_other_adjust_mdot = .false. !Turn off the  accretion
              s% use_other_wind = .false.  !No winds during accretion
              write(*,*) 'use_other_wind =.false. '
              s% Dutch_scaling_factor = 1.0d0
              s% Blocker_scaling_factor = 0.2d0
              s% Reimers_scaling_factor = 0.1d0
              !write(*,*) 'Dutch_scaling_factor= ', s% Dutch_scaling_factor
              !write(*,*) 'Blocker_scaling_factor= ', s% Blocker_scaling_factor 
              !write(*,*) 'Reimers_scaling_factor= ',s% Reimers_scaling_factor 

            end if
    
      end subroutine SMS_adjust_mdot
 



      end module CE_SMS_accretion
