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

      module CE_torque

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
      use CE_orbit, only: AtoP, TukeyWindow, calc_quantities_at_comp_position

      implicit none

      contains

! ***********************************************************************
      subroutine CE_inject_am(id, ierr)
      ! Angular momentum prescription from CE
      ! Based off example from mesa/star/other/other_torque.f

         use const_def, only: Rsun
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k, k_bottom
         real(dp) :: a_tukey, mass_to_be_spun, ff
         real(dp) :: CE_companion_position, CE_companion_mass, CE_n_acc_radii, CE_torque
         real(dp) :: R_acc, R_acc_low, R_acc_high
         real(dp) :: v_rel, v_rel_div_csound, M_encl, rho_at_companion, scale_height_at_companion

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         CE_companion_position = s% xtra(2)
         CE_companion_mass = s% xtra(4)

         ! If the star is in the initial relaxation phase, skip torque calculations
         if (s% doing_relax) return
         ! If companion is outside star, skip torque calculations
         if (CE_companion_position*Rsun > s% r(1)) return

         ! If system merged, skip energy deposition
         if (s% lxtra(1)) then
            s% extra_jdot(:) = 0.0d0
            return
         endif

         ! Load angular momentum dissipated in the envelope from the orbit decrease
         CE_n_acc_radii = s% xtra(5)
         CE_torque = s% xtra(6)

         call calc_quantities_at_comp_position(id, ierr)

         R_acc = s% xtra(12)
         R_acc_low = s% xtra(13)
         R_acc_high = s% xtra(14)
         M_encl = s% xtra(15)
         v_rel = s% xtra(16)
         v_rel_div_csound = s% xtra(17)
         rho_at_companion = s% xtra(18)
         scale_height_at_companion = s% xtra(19)

         ! Tukey window scale
         a_tukey = 0.5

         ! First calculate the mass in which the angular momentum will be deposited
         mass_to_be_spun = 0.0
         do k = 1, s% nz
            if (s% r(k) < CE_companion_position*Rsun) then
               R_acc = R_acc_low
            else
               R_acc = R_acc_high
            end if
            ff = TukeyWindow((s% r(k) - CE_companion_position*Rsun)/(CE_n_acc_radii * 2.0 * R_acc), a_tukey)
            mass_to_be_spun = mass_to_be_spun + s% dm(k) * ff
            !Energy should be deposited only on the envelope of the star and not in the core
            !When we reach the core boundary we exit the loop
            if (s% m(k) < s% he_core_mass * Msun) exit
         end do
         !this is the limit in k of the boundary between core and envelope
         k_bottom = k-1
         ! If companion is outside star, set mass_to_be_heated arbitrarily low
         if (mass_to_be_spun == 0.) mass_to_be_spun = 1.0

         ! Now redo the loop and add the extra torque
         do k = 1, s% nz
            if (s% r(k) < CE_companion_position*Rsun) then
               R_acc = R_acc_low
            else
               R_acc = R_acc_high
            end if
            ff = TukeyWindow((s% r(k) - CE_companion_position*Rsun)/(CE_n_acc_radii * 2.0 * R_acc), a_tukey)
            s% extra_jdot(k) = CE_torque / mass_to_be_spun * ff
         end do
      end subroutine CE_inject_am


! ***********************************************************************
      subroutine CE_inject_am2(id, ierr)

         use const_def, only: Rsun
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k, k_bottom
         real(dp) :: a_tukey, volume_to_be_spun, ff
         real(dp) :: CE_companion_position, CE_companion_mass, CE_n_acc_radii, CE_torque
         real(dp) :: R_acc, R_acc_low, R_acc_high
         real(dp), DIMENSION(:), ALLOCATABLE :: cell_dr
         real(dp) :: v_rel, v_rel_div_csound, M_encl, rho_at_companion, scale_height_at_companion

         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         CE_companion_position = s% xtra(2)
         CE_companion_mass = s% xtra(4)

         ! If the star is in the initial relaxation phase, skip torque calculations
         if (s% doing_relax) return
         ! If companion is outside star, skip torque calculations
         if (CE_companion_position*Rsun > s% r(1)) return

         ! If system merged, skip energy deposition
         if (s% lxtra(1)) then
            s% extra_jdot(:) = 0.0d0
            return
         endif

         ! Load angular momentum dissipated in the envelope from the orbit decrease
         CE_n_acc_radii = s% xtra(5)
         CE_torque = s% xtra(6)

         call calc_quantities_at_comp_position(id, ierr)

         R_acc = s% xtra(12)
         R_acc_low = s% xtra(13)
         R_acc_high = s% xtra(14)
         M_encl = s% xtra(15)
         v_rel = s% xtra(16)
         v_rel_div_csound = s% xtra(17)
         rho_at_companion = s% xtra(18)
         scale_height_at_companion = s% xtra(19)

         ! Tukey window scale
         a_tukey = 0.5

         ! define cell width
         allocate(cell_dr(s% nz))
         do k = 2, s% nz
            cell_dr(k-1) = s% rmid(k-1) - s% rmid(k)
         end do
         cell_dr(s% nz) = s% rmid(s% nz) - s% R_center

         ! First calculate the mass in which the angular momentum will be deposited
         volume_to_be_spun = 0.0
         do k = 1, s% nz
            if (s% r(k) < CE_companion_position*Rsun) then
               R_acc = R_acc_low
            else
               R_acc = R_acc_high
            end if
            ff = TukeyWindow((s% r(k) - CE_companion_position*Rsun)/(CE_n_acc_radii * 2.0 * R_acc), a_tukey)
            volume_to_be_spun = volume_to_be_spun + 4.0d0 * pi * s% r(k) * s% r(k) * cell_dr(k) * ff
            !Energy should be deposited only on the envelope of the star and not in the core
            !When we reach the core boundary we exit the loop
            if (s% m(k) < s% he_core_mass * Msun) exit
         end do
         !this is the limit in k of the boundary between core and envelope
         k_bottom = k-1
         ! If companion is outside star, set mass_to_be_heated arbitrarily low
         if (volume_to_be_spun == 0.) volume_to_be_spun = 1.0

         ! Now redo the loop and add the extra torque
         do k = 1, s% nz
            if (s% r(k) < CE_companion_position*Rsun) then
               R_acc = R_acc_low
            else
               R_acc = R_acc_high
            end if
            ff = TukeyWindow((s% r(k) - CE_companion_position*Rsun)/(CE_n_acc_radii * 2.0 * R_acc), a_tukey)
            s% extra_jdot(k) = CE_torque * (4.0d0 * pi * s% r(k) * s% r(k) * cell_dr(k) * ff / volume_to_be_spun) / s% dm(k)
         end do
         deallocate(cell_dr)
      end subroutine CE_inject_am2


      end module CE_torque
