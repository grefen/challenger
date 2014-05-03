/*
  Challenger, a UCI chess playing engine derived from Stockfish
  
  

  Challenger is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Challenger is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef NOTATION_H_INCLUDED
#define NOTATION_H_INCLUDED

#include <string>

#include "types.h"

class Position;

std::string score_to_uci(Value v, Value alpha = -VALUE_INFINITE, Value beta = VALUE_INFINITE);
Move move_from_uci(const Position& pos, std::string& str);
const std::string move_to_uci(Move m, bool chess960);
const std::string move_to_san(Position& pos, Move m);
std::string pretty_pv(Position& pos, int depth, Value score, int64_t msecs, Move pv[]);

extern std::string move_to_chinese(const Position& pos, Move m);

#endif // #ifndef NOTATION_H_INCLUDED
