!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!### Description
!  Numeric kind definitions for BSpline-Fortran.

    module bspline_kinds_module

    use,intrinsic :: iso_fortran_env

    implicit none

    private

    integer,parameter,public :: wp = real32   !! Real working precision [8 bytes]

    integer,parameter,public :: ip = int32    !! Integer working precision [4 bytes]

!*****************************************************************************************
    end module bspline_kinds_module
!*****************************************************************************************
