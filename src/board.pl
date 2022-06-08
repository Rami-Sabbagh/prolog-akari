:- module(board, [
    title/1, % title(Title)
    size/2, % size(Columns, Rows)

    wall/2, % wall(Column, Row)
    wall_num/3, %wall_num(Column, Row, Num)
    light/2, % light(Column, Row)

    restricted/2 % restricted(Column, Row) (marked as light not allowed (the dot)).
]).

:- 
    dynamic(title/1),
    dynamic(size/2), 
    dynamic(wall/2), 
    dynamic(wall_num/3), 
    dynamic(light/2),
    dynamic(restricted/2).
