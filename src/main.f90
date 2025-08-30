module mod_vector3
   implicit none

   type :: vector3_t
      real :: x, y, z
   end type vector3_t

contains

   function vector3_zeros() result(out)
      type(vector3_t) :: out
      out = vector3_build(0., 0., 0.)
   end function vector3_zeros

   function vector3_build(x, y, z) result(out)
      real, intent(in) :: x, y, z
      type(vector3_t) :: out

      out%x = x
      out%y = y
      out%z = z
   end function vector3_build

   function vector3_add(a, b) result(out)
      type(vector3_t), intent(in) :: a, b
      type(vector3_t) :: out

      out%x = a%x + b%x
      out%y = a%y + b%y
      out%z = a%z + b%z
   end function vector3_add

   function vector3_sub(a, b) result(out)
      type(vector3_t), intent(in) :: a, b
      type(vector3_t) :: out

      out%x = a%x - b%x
      out%y = a%y - b%y
      out%z = a%z - b%z
   end function vector3_sub

   function vector3_mul(vec, scalar) result(out)
      type(vector3_t), intent(in) :: vec
      real, intent(in) :: scalar
      type(vector3_t) :: out

      out%x = vec%x*scalar
      out%y = vec%y*scalar
      out%z = vec%z*scalar
   end function vector3_mul

   function vector3_div(vec, scalar) result(out)
      type(vector3_t), intent(in) :: vec
      real, intent(in) :: scalar
      type(vector3_t) :: out

      out%x = vec%x/scalar
      out%y = vec%y/scalar
      out%z = vec%z/scalar
   end function vector3_div

   function vector3_mag_sq(vec) result(out)
      type(vector3_t), intent(in) :: vec
      real :: out

      out = vec%x*vec%x + vec%y*vec%y + vec%z*vec%z
   end function vector3_mag_sq

   function vector3_magnitude(vec) result(out)
      type(vector3_t), intent(in) :: vec
      real :: out

      out = sqrt(vec%x*vec%x + vec%y*vec%y + vec%z*vec%z)
   end function vector3_magnitude

   function vector3_normalize(vec) result(out)
      type(vector3_t), intent(in) :: vec
      type(vector3_t) :: out

      out = vector3_div(vec, vector3_magnitude(vec))
   end function vector3_normalize

   function vector3_dot(a, b) result(out)
      type(vector3_t), intent(in) :: a, b
      type(vector3_t) :: out

      out%x = a%x*b%x
      out%y = a%y*b%y
      out%z = a%z*b%z
   end function vector3_dot

   function vector3_cross(a, b) result(out)
      type(vector3_t), intent(in) :: a, b
      type(vector3_t) :: out

      out%x = a%y*b%z - a%z*b%y
      out%y = a%z*b%x - a%x*b%z
      out%z = a%x*b%y - a%y*b%z
   end function vector3_cross

   function vector3_rotate_x(vector, angle) result(out)
      type(vector3_t), intent(in) :: vector
      real, intent(in) :: angle
      type(vector3_t) :: out
      real :: s, c

      s = sin(angle)
      c = cos(angle)

      out%x = vector%x
      out%y = vector%y*c - vector%z*s
      out%z = vector%y*s + vector%z*c
   end function vector3_rotate_x

   function vector3_rotate_y(vector, angle) result(out)
      type(vector3_t), intent(in) :: vector
      real, intent(in) :: angle
      type(vector3_t) :: out
      real :: s, c

      s = sin(angle)
      c = cos(angle)

      out%x = vector%x*c + vector%z*s
      out%y = vector%y
      out%z = -vector%x*s + vector%z*c
   end function vector3_rotate_y

   function vector3_rotate_z(vector, angle) result(out)
      type(vector3_t), intent(in) :: vector
      real, intent(in) :: angle
      type(vector3_t) :: out
      real :: s, c

      s = sin(angle)
      c = cos(angle)

      out%x = vector%x*c - vector%y*s
      out%y = vector%x*s + vector%y*c
      out%z = vector%z
   end function vector3_rotate_z

   function vector3_rotate_xyz(vector, angles) result(out)
      type(vector3_t), intent(in) :: vector
      type(vector3_t), intent(in) :: angles
      type(vector3_t) :: out

      out = vector
      out = vector3_rotate_x(out, angles%x)
      out = vector3_rotate_y(out, angles%y)
      out = vector3_rotate_z(out, angles%z)
   end function vector3_rotate_xyz

end module mod_vector3

module mod_camera
   use mod_vector3
   implicit none

   type :: camera_t
      type(vector3_t) :: position
      type(vector3_t) :: front
      type(vector3_t) :: right
      type(vector3_t) :: up
   end type camera_t

contains

   function camera_build(position) result(out)
      type(vector3_t), intent(in) :: position
      type(camera_t) :: out
   end function camera_build

end module mod_camera

module mod_buffer
   use mod_vector3
   implicit none

   type :: buffer_t
      integer :: height, width
      character(len=1), allocatable :: pixels(:)
      real, allocatable :: depth(:)
   end type buffer_t

contains

   function buffer_build(width, height) result(out)
      integer, intent(in) :: width, height
      type(buffer_t) :: out
      integer :: size

      size = height*width
      out%height = height
      out%width = width
      allocate (out%pixels(size))
      allocate (out%depth(size))
   end function buffer_build

   function buffer_access(self, x, y) result(out)
      type(buffer_t), intent(inout), target  :: self
      integer, intent(in) :: x, y
      character(len=1), pointer :: out
      integer :: idx

      idx = self%width*y + x + 1
      out => self%pixels(idx)
   end function buffer_access

   subroutine buffer_set(self, x, y, char, depth)
      type(buffer_t), intent(inout), target  :: self
      integer, intent(in) :: x, y
      character(len=1), intent(in) :: char
      real, intent(in) :: depth
      integer :: idx

      if (x < 0 .or. x >= self%width) then
         return
      end if
      if (y < 0 .or. y >= self%height) then
         return
      end if

      idx = self%width*y + x + 1
      if (depth < self%depth(idx)) then
         self%depth(idx) = depth
         self%pixels(idx) = char
      end if
   end subroutine buffer_set

   subroutine buffer_clear(self, char)
      type(buffer_t), intent(inout) :: self
      character(len=1), intent(in) :: char
      integer :: index

      do index = 1, size(self%pixels)
         self%pixels(index) = char
         self%depth(index) = 1.0e9
      end do
   end subroutine buffer_clear

   subroutine buffer_display(self)
      type(buffer_t), intent(inout) :: self
      integer :: x, y

      call system("cls")
      do y = 0, self%height - 1
         do x = 0, self%width - 1
            write (*, "(A1)", advance="no") buffer_access(self, x, y)
         end do
         write (*, *)
      end do
   end subroutine buffer_display

end module mod_buffer

module mod_cube
   use mod_vector3
   implicit none

   type :: cube_t
      type(vector3_t) :: position
      type(vector3_t) :: rotation
      real :: size
   end type cube_t

contains

   function cube_build(position, size) result(out)
      type(vector3_t), intent(in) :: position
      real, intent(in) :: size
      type(cube_t) :: out

      out%position = position
      out%size = size
   end function cube_build

   subroutine cube_rotate(self, rotation)
      type(cube_t), intent(inout) :: self
      type(vector3_t), intent(in) :: rotation
      self%rotation = vector3_add(self%rotation, rotation)
   end subroutine cube_rotate

end module mod_cube

module mod_renderer
   use mod_vector3
   use mod_buffer
   use mod_camera
   use mod_cube
   implicit none

contains

   subroutine render_cube(buffer, camera, cube)
      type(buffer_t), intent(inout) :: buffer
      type(camera_t), intent(inout) :: camera
      type(cube_t), intent(inout) :: cube
      real :: step, x, y, z

      step = 1.0

      do x = -cube%size, cube%size, step
         do y = -cube%size, cube%size, step
            z = cube%size
            call render_point(buffer, camera, cube, x, y, z, ".")
            call render_point(buffer, camera, cube, x, y, -z, "?")
            call render_point(buffer, camera, cube, x, z, y, "%")
            call render_point(buffer, camera, cube, x, -z, y, "@")
            call render_point(buffer, camera, cube, z, x, y, ";")
            call render_point(buffer, camera, cube, -z, x, y, "*")
         end do
      end do
   end subroutine render_cube

   subroutine render_point(buffer, camera, cube, x, y, z, char)
      type(buffer_t), intent(inout) :: buffer
      type(camera_t), intent(in) :: camera
      type(cube_t), intent(in) :: cube
      real, intent(in) :: x, y, z
      character(len=1), intent(in) :: char

      real :: screenx, screeny
      integer :: intx, inty
      type(vector3_t) :: point

      point = vector3_build(x, y, z)
      point = vector3_rotate_xyz(point, cube%rotation)
      point = vector3_add(point, cube%position)
      point = vector3_sub(point, camera%position)

      if (point%z < 0.1) then
         return
      end if

      screenx = real(buffer%width)/2.0 + 100.0*point%x/point%z
      screeny = real(buffer%height)/2.0 - 100.0*0.7*point%y/point%z

      intx = int(screenx)
      inty = int(screeny)

      call buffer_set(buffer, intx, inty, char, point%z)
   end subroutine render_point

end module mod_renderer

program main
   use mod_vector3
   use mod_buffer
   use mod_camera
   use mod_renderer
   use mod_cube
   implicit none

   type(buffer_t) :: buffer
   type(camera_t) :: camera
   type(cube_t) :: cube

   cube = cube_build(vector3_build(0., 0., 70.), 10.)
   camera%position = vector3_zeros()
   buffer = buffer_build(120, 70)

   do
      call buffer_clear(buffer, " ")
      call render_cube(buffer, camera, cube)
      call buffer_display(buffer)
      call cube_rotate(cube, vector3_build(0.12, 0.09, 0.05))
   end do
end program main

