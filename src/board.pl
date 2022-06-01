% https://krazydad.com/play/akari/?kind=8x8&volumeNumber=1&bookNumber=1&puzzleNumber=1

size(8, 8).

wall(1,2).
wall(1,3).
wall(1,6).
wall(1,7).

wall(3,2).
wall(3,3).
wall(3,6).
wall(3,7).

wall(6,2).
wall(6,3).
wall(6,6).
wall(6,7).

wall(8,2).
wall(8,3).
wall(8,6).
wall(8,7).

wall_num(1, 2, 1).
wall_num(1, 6, 1).
wall_num(1, 7, 2).
wall_num(3, 6, 1).
wall_num(6, 3, 1).
wall_num(6, 7, 2).

% Solution
light(1, 1).
light(1, 5).
light(1, 8).
light(2, 7).
light(4, 6).
light(5, 7).
light(6, 4).
light(7, 7).