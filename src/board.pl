:- module(board, [
    title/1, % title(Title)
    size/2, % size(Rows, Columns)

    wall/2, % wall(Row, Column)
    wall_num/3, %wall_num(Row, Column, Num)
    light/2, % light(Row, Column)

    restricted/2 % restricted(Row, Column) (marked as light not allowed (the dot)).
]).

:- 
    dynamic(title/1),
    dynamic(size/2), 
    dynamic(wall/2), 
    dynamic(wall_num/3), 
    dynamic(light/2),
    dynamic(restricted/2).
