! gfortran -O3 -fopenmp 20.f90 -o 20
program p20
   implicit none

   character(1), allocatable :: grid(:)
   integer :: n_rows, n_cols, n, start, end_pos
   integer, allocatable :: dist_from_start(:), dist_to_end(:)
   integer :: i, part1, part2

   block
      integer :: unit=10, ios
      character(len=1000) :: line
      open(unit, file='input/input_20.txt', status='old')
      read(unit, '(A)') line
      n_cols = len_trim(line)
      n_rows = 0
      rewind(unit)
      do
         read(unit, *, iostat=ios)
         if (ios /= 0) exit
         n_rows = n_rows + 1
      end do

      n = n_rows * n_cols
      allocate(grid(n))
      rewind(unit)
      i = 1
      do while (i <= n)
         read(unit, '(A)') line
         grid(i:i+n_cols-1) = transfer(line(1:n_cols), grid, n_cols)
         i = i + n_cols
      end do
      close(unit)
   end block

   ! find S to E
   start = findloc(grid, 'S', dim=1)
   end_pos = findloc(grid, 'E', dim=1)

   allocate(dist_from_start(n), dist_to_end(n))
   dist_from_start = bfs(start, .false.)
   dist_to_end = bfs(end_pos, .false.)

   if (dist_from_start(end_pos) == -1) then
      print *, 'error: no valid path from start to end'
      stop
   end if

   part1 = count_cheats(2)
   part2 = count_cheats(20)
   print '("Part 1: ",I0)', part1
   print '("Part 2: ",I0)', part2

contains
   function bfs(start, allow_walls) result(dist)
      integer, intent(in) :: start
      logical, intent(in) :: allow_walls
      integer :: dist(n), queue(n), head, tail, curr, nb(4), nb_count, next
      integer :: r, c, i

      dist = -1
      dist(start) = 0
      head = 1
      tail = 1
      queue(tail) = start

      do while (head <= tail)
         curr = queue(head)
         head = head + 1

         r = (curr-1) / n_cols + 1
         c = mod(curr-1, n_cols) + 1
         nb_count = 0

         if (r > 1) nb(nb_count+1) = curr - n_cols
         if (r < n_rows) nb(nb_count+2) = curr + n_cols
         if (c > 1) nb(nb_count+3) = curr - 1
         if (c < n_cols) nb(nb_count+4) = curr + 1
         nb_count = count([r>1, r<n_rows, c>1, c<n_cols])

         do i = 1, nb_count
            next = nb(i)
            if (dist(next) == -1 .and. (allow_walls .or. grid(next) /= '#')) then
               dist(next) = dist(curr) + 1
               tail = tail + 1
               queue(tail) = next
            end if
         end do
      end do
   end function

   function count_cheats(max_length) result(total)
      integer, intent(in) :: max_length
      integer :: total, x, dist_cheat(n), queue(n)
      integer :: head, tail, curr, steps, nb(4), nb_count
      integer :: r, c, i, next, y

      total = 0
      !$OMP PARALLEL DO REDUCTION(+:total) PRIVATE(dist_cheat,queue,head,tail,curr,steps,nb,nb_count,r,c,i,next,y)
      do x = 1, n
         if (grid(x) == '#' .or. dist_from_start(x) == -1) cycle

         dist_cheat = -1
         dist_cheat(x) = 0
         head = 1
         tail = 1
         queue(tail) = x

         do while (head <= tail)
            curr = queue(head)
            steps = dist_cheat(curr)
            head = head + 1
            if (steps >= max_length) cycle

            r = (curr-1) / n_cols + 1
            c = mod(curr-1, n_cols) + 1
            nb_count = 0

            if (r > 1) nb(nb_count+1) = curr - n_cols
            if (r < n_rows) nb(nb_count+2) = curr + n_cols
            if (c > 1) nb(nb_count+3) = curr - 1
            if (c < n_cols) nb(nb_count+4) = curr + 1
            nb_count = count([r>1, r<n_rows, c>1, c<n_cols])

            do i = 1, nb_count
               next = nb(i)
               if (dist_cheat(next) == -1) then
                  dist_cheat(next) = steps + 1
                  tail = tail + 1
                  queue(tail) = next
               end if
            end do
         end do

         do i = 1, tail
            y = queue(i)
            if (grid(y) /= '#' .and. dist_to_end(y) /= -1) then
               if (dist_from_start(end_pos) - (dist_from_start(x) + dist_cheat(y) + dist_to_end(y)) >= 100) then
                  total = total + 1
               end if
            end if
         end do
      end do
      !$OMP END PARALLEL DO
   end function
end program p20
