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

#include <cassert>
#include <algorithm>
#include <cassert>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <sstream>

#include "movegen.h"
#include "position.h"
#include "notation.h"

/// Simple macro to wrap a very common while loop, no facny, no flexibility,
/// hardcoded names 'mlist' and 'from'.
#define SERIALIZE(b) while (b) (mlist++)->move = make_move(from, pop_lsb(&b))

/// Version used for pawns, where the 'from' square is given as a delta from the 'to' square
#define SERIALIZE_PAWNS(b, d) while (b) { Square to = pop_lsb(&b); \
                                         (mlist++)->move = make_move(to - (d), to); }
namespace {

 
  template<Color Us, GenType Type>
  ExtMove* generate_pawn_moves(const Position& pos, ExtMove* mlist,
                               Bitboard target, const CheckInfo* ci) {

    // Compute our parametrized parameters at compile time, named according to
    // the point of view of white side.
    const Color    Them     = (Us == WHITE ? BLACK    : WHITE);
    const Square   Up       = (Us == WHITE ? DELTA_N  : DELTA_S);
	const Square   Right    = (Us == WHITE ? DELTA_E : DELTA_W);
    const Square   Left     = (Us == WHITE ? DELTA_W : DELTA_E);
	const Bitboard MaskBB   =  PawnMask[Us];

    Bitboard b1, b2, b3, dc1, dc2, dc3, emptySquares;

    Bitboard enemies = (Type == EVASIONS ? pos.pieces(Them) & target:
                        Type == CAPTURES ? target : pos.pieces(Them));

	Bitboard pawns   = pos.pieces(Us, PAWN) & MaskBB;
	Bitboard passedRiver = pawns & PassedRiverBB[Us];

	//target:针对不同的move类型，有不同的目的   
    if (Type != CAPTURES)
    {
        emptySquares = (Type == QUIETS || Type == QUIET_CHECKS ? target : ~pos.pieces());

		b1 = shift_bb<Up>(pawns) & emptySquares;
		b2 = shift_bb<Left>(pawns) & emptySquares;
		b3 = shift_bb<Right>(pawns) & emptySquares;

        if (Type == EVASIONS) // Consider only blocking squares
        {
            b1 &= target;
            b2 &= target;
			b3 &= target;
        }

        if (Type == QUIET_CHECKS)
        {
          
			b1 &= pos.attacks_from_pawn_nomask(ci->ksq, Them) & PassedRiverBB[Them];
			b2 &= pos.attacks_from_pawn_nomask(ci->ksq, Them) & PassedRiverBB[Them];
			b3 &= pos.attacks_from_pawn_nomask(ci->ksq, Them) & PassedRiverBB[Them];

			if (passedRiver & ci->dcCandidates)
            {
                //被车牵制，被炮牵制，被马牵制
				//被马牵制，任何位置都可以
				//被车牵制，不与king同行同列
				//被炮牵制，不与king同行同列

				dc1 = shift_bb<Up>(passedRiver & ci->dcCandidates) & emptySquares;
                dc2 = shift_bb<Left>(passedRiver & ci->dcCandidates) & emptySquares;
				dc3 = shift_bb<Right>(passedRiver & ci->dcCandidates) & emptySquares;

                b1 |= dc1;
                b2 |= dc2;
				b3 |= dc3;
            }
			else
			{
				//充当炮架
				dc1 = shift_bb<Up>(passedRiver) & emptySquares & ci->forbid;
				dc2 = shift_bb<Left>(passedRiver) & emptySquares & ci->forbid;
				dc3 = shift_bb<Right>(passedRiver) & emptySquares & ci->forbid;

				b1 |= dc1;
				b2 |= dc2;
				b3 |= dc3;
			}
        }

		b1 &= MaskBB;
		b2 &= MaskBB;
		b3 &= MaskBB;


        SERIALIZE_PAWNS(b1, Up);
        SERIALIZE_PAWNS(b2, Left);
		SERIALIZE_PAWNS(b3, Right);
    }

    // Standard and captures
    if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
    {
        
		b1 = shift_bb<Right>(pawns) & enemies;
        b2 = shift_bb<Left >(pawns) & enemies;
		b3 = shift_bb<Up >(pawns) & enemies;

		b1 &= MaskBB;
		b2 &= MaskBB;
		b3 &= MaskBB;

        SERIALIZE_PAWNS(b1, Right);
        SERIALIZE_PAWNS(b2, Left);
		SERIALIZE_PAWNS(b3, Up);
    }

    return mlist;
  }


 /* template<PieceType Pt, bool Checks> FORCE_INLINE
  ExtMove* generate_moves(const Position& pos, ExtMove* mlist, Color us,
                          Bitboard target, const CheckInfo* ci) {

    assert(Pt != KING && Pt != PAWN);

    const Square* pl = pos.list<Pt>(us);

    for (Square from = *pl; from != SQ_NONE; from = *++pl)
    {
        if (Checks)
        {
            if (    (Pt == BISHOP || Pt == ROOK || Pt == QUEEN)
                && !(PseudoAttacks[Pt][from] & target & ci->checkSq[Pt]))
                continue;

            if (unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
                continue;
        }

        Bitboard b = pos.attacks_from<Pt>(from) & target;

        if (Checks)
            b &= ci->checkSq[Pt];

        SERIALIZE(b);
    }

    return mlist;
  }*/

  template<PieceType Pt, bool Checks> FORCE_INLINE
	  ExtMove* generate_moves(const Position& pos, ExtMove* mlist, Color us,
	  Bitboard target, const CheckInfo* ci)
  {

	  assert(Pt != KING && Pt != PAWN);

	  //rook,cannon,knight,bishop，advisor

	  const Square* pl = pos.list<Pt>(us);

	  for (Square from = *pl; from != SQ_NONE; from = *++pl)
	  {
		  //Checks表示quiet check的意思,中国象棋中下面方法不成立
		  if (Checks)
		  {
			  ////不能直接将军
			  //if ( (Pt == ROOK || Pt == CANNON)
				 // && !(PseudoAttacks[Pt][from] & target & ci->checkSq[Pt]))
				 // continue;

			  ////Checks表示quiet check的意思，但是这种情况在调用这个函数之前已经处理
			  //if (unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
				 // continue;

			  //这两个子不会直接将军，但是可以利用炮非吃子将军
			  //if(Pt == BISHOP || Pt == ADVISOR)
				 // continue;
		  }

		  Bitboard b;

		  if(Pt == ADVISOR )
		  {
			  b = pos.attacks_from<ADVISOR>(from, us) & target;
		  }
		  else if(Pt == BISHOP)
		  {
			  b = pos.attacks_from<BISHOP>(from, us) & target;
		  }
		  else if(Pt == CANNON)
		  {
			  b = pos.attacks_from<ROOK>(from) & target & (~pos.pieces());//非吃子，上一层中，target也要控制吃子和非吃子，所以这里有非吃子计算也是正确的
			 
			  b |= pos.attacks_from<CANNON>(from) & target & pos.pieces();//吃子
			 
		  }
		  else if(Pt == ROOK)
		  {
			  b = pos.attacks_from<ROOK>(from) & target;	
		  }
		  else if(Pt == KNIGHT)
		  {
			  b = pos.attacks_from<KNIGHT>(from) & target;
		  }

		  //checkSq[Pt]表示Pt可以check对方king的地方，这里是直接check，对直接check的子有用,炮情况特殊要特别考虑
		  //对于非直接check的子，可以当炮架，可以当阻挡rook check的子
		  //这种在使用之前已经计算好了，可以增加速度
		  //对于直接check的子，b &= ci->checkSq[Pt]增加速度，防止每个字都调用check的计算
		  //对于非直接check的子,要check计算
		  if (Checks)
		  {
			  //对于炮有问题，如 -*-c-----k---, c移动到*处会被认为是quiet check
			  if(Pt == CANNON)
			  {
				  if(unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
				  {
					  //被车或炮或马牵制
					  //被车牵制，不能与king同行同列
					  //被炮牵制，不能与king同行同列
                      //马牵制，任何位置都可以                     

					  //如果from不与ci.ksq同行或同列，则是被马牵制
					  if(PseudoAttacks[ROOK][ci->ksq] & from)
					  {
						  //与king 同行或同列，必定是被车炮牵制
						  b &= ~PseudoAttacks[ROOK][ci->ksq];
					  }
					  //否则是被马牵制，任何位置都可以

				  }
				  else
				  {
				      //如果与king同行同列，那么非吃子的任何位置都不可能quiet check
                      if( !(PseudoAttacks[ROOK][ci->ksq] & from))
					  {
						  //from 不与king同行同列，则下面的条件成立
                          b &= ci->checkSq[Pt]|ci->forbid;//可作为炮架
					  }
					  else
					  {
						  //与king同行同列，并且不是被牵制，则任何位置都不可能是quiet check
						  b = Bitboard();
					  }
					  
				  }
			  }
			  else if(Pt == ROOK)
			  {
				  if(unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
				  {
					  //被炮或马牵制
					  //马牵制，任何位置都可以
					  //炮牵制，不能与对方的king同行或同列

					  if(PseudoAttacks[ROOK][ci->ksq] & from)
					  {
						  //与king同行同列，必定被炮或车牵制
						  b &= ~PseudoAttacks[ROOK][ci->ksq];
					  }
					  //否则被马牵制，任何位置都可以
				  }
				  else
				  {
					  b &= ci->checkSq[Pt];//已经包含了炮架
				  }
			  }
			  else if(Pt == KNIGHT)
			  {
				  if(unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
				  {
					  //被车或炮或马牵制
					  //任何位置都是check
				  }
				  else
				  {
					  b &= ci->checkSq[Pt]|ci->forbid;//check或炮架
				  }
			  }
			  else if(Pt == BISHOP || Pt == ADVISOR)
			  {
				  //被车，炮牵制
				  //任何位置都可以
                  if(unlikely(ci->dcCandidates) && (ci->dcCandidates & from))
				  {
				  }
				  else
				  {
					  //否则必须充当炮架
                      b &= ci->forbid;
				  }
			  }
		  }

		  SERIALIZE(b);
	  }

	  return mlist;
  }


  template<Color Us, GenType Type> FORCE_INLINE
  ExtMove* generate_all(const Position& pos, ExtMove* mlist, Bitboard target,
                        const CheckInfo* ci = NULL) {

    const bool Checks = Type == QUIET_CHECKS;

	Bitboard fbbtarget = (~pos.cannon_forbid_bb(Us)) & target;//不能当炮架

    mlist = generate_pawn_moves<Us, Type>(pos, mlist, fbbtarget, ci);
    mlist = generate_moves<KNIGHT, Checks>(pos, mlist, Us, fbbtarget, ci);
    mlist = generate_moves<BISHOP, Checks>(pos, mlist, Us, fbbtarget, ci);
    mlist = generate_moves<  ROOK, Checks>(pos, mlist, Us, fbbtarget, ci);
	mlist = generate_moves<CANNON, Checks>(pos, mlist, Us, fbbtarget, ci);
	mlist = generate_moves<ADVISOR, Checks>(pos, mlist, Us, fbbtarget, ci);

    //EVASIONS 调用前已经处理
	//QUIET_CHECKS ?调用前已经处理
	if (Type != QUIET_CHECKS && Type != EVASIONS)
    {
        Square from = pos.king_square(Us);
        Bitboard b = pos.attacks_from<KING>(from, Us) & target;
        SERIALIZE(b);
    }

    return mlist;
  }


} // namespace


/// generate<CAPTURES> generates all pseudo-legal captures and queen
/// promotions. Returns a pointer to the end of the move list.
///
/// generate<QUIETS> generates all pseudo-legal non-captures and
/// underpromotions. Returns a pointer to the end of the move list.
///
/// generate<NON_EVASIONS> generates all pseudo-legal captures and
/// non-captures. Returns a pointer to the end of the move list.

template<GenType Type>
ExtMove* generate(const Position& pos, ExtMove* mlist) {

  assert(Type == CAPTURES || Type == QUIETS || Type == NON_EVASIONS);
  assert(!pos.checkers());

  Color us = pos.side_to_move();

  Bitboard target = Type == CAPTURES     ?  pos.pieces(~us)
                  : Type == QUIETS       ? ~pos.pieces()
                  : Type == NON_EVASIONS ? ~pos.pieces(us) : Bitboard();

  return us == WHITE ? generate_all<WHITE, Type>(pos, mlist, target)
                     : generate_all<BLACK, Type>(pos, mlist, target);
}

// Explicit template instantiations
template ExtMove* generate<CAPTURES>(const Position&, ExtMove*);
template ExtMove* generate<QUIETS>(const Position&, ExtMove*);
template ExtMove* generate<NON_EVASIONS>(const Position&, ExtMove*);


/// generate<QUIET_CHECKS> generates all pseudo-legal non-captures and knight
/// underpromotions that give check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<QUIET_CHECKS>(const Position& pos, ExtMove* mlist) {

  assert(!pos.checkers());

  Color us = pos.side_to_move();
  CheckInfo ci(pos);
  Bitboard dc = ci.dcCandidates;
  Square from = pos.king_square(us);

  //被牵制隐含的check,被车，马，炮牵制的子
  //国际象棋中cCandidates可能是pawn，kinght，bishop,king,rook不可能是queen，因这直接将军
  //pawn如果是Candidates,只能是pawn自己的check或queen,bishop,rook check
  //knight,走哪里都可以产生check
  //bishop,走哪里都可以产生check
  //rook,走哪里都可以产生check
  //queen,只能直接check
  //king,要不与对方king同方向运动
  //国际象棋中下面是成立的
  //中国象棋中情况复杂，下面的不成立
  //while (dc)
  //{
  //   Square from = pop_lsb(&dc);
  //   PieceType pt = type_of(pos.piece_on(from));

  //   //这个也可以quiet check，后面处理,可提高速度
	 //if (pt == PAWN)
  //       continue; // Will be generated togheter with direct checks

  //   //Bitboard b = pos.attacks_from(Piece(pt), from) & ~pos.pieces();

	 //Bitboard b;

	 ////if(pt == CANNON)
	 ////{         
		//// //如果是炮，炮移动方向不能与对方king同一方向，如果是同一方向，只能是吃子才能将军，与假设不服
		//// //但是炮可能是马腿
		//// b = pos.attacks_from<ROOK>(from) & ~pos.pieces();
		//// //b &= ~PseudoAttacks[ROOK][ci.ksq];//king方向上的移动,但炮可能是马腿
	 ////}
	 ////else
	 ////{
		//// b = pos.attacks_from(pos.piece_on(from), from) & ~pos.pieces();
	 ////}

	 //if(pt == ADVISOR)
	 //{
  //      b = pos.attacks_from(pos.piece_on(from), from) & ~pos.pieces();
	 //}
	 //else if(pt == BISHOP)
	 //{
		//b = pos.attacks_from(pos.piece_on(from), from) & ~pos.pieces();
	 //}
	 //else if (pt == KING)
	 //{
		// b &= ~PseudoAttacks[ROOK][ci.ksq];//只能是cannon牵制，不能与对方king在一条线上;如果这里没有king，不许gen king的quiet check，所以后面把该情况排除在外了
	 //}

  //   SERIALIZE(b);
  //}

  //上面只有king一种情况，所以可以直接判断king是不是dcCandidates
  if(dc&from)
  {
        Bitboard b = pos.attacks_from(pos.piece_on(from), from) & ~pos.pieces();

		b &= ~PseudoAttacks[ROOK][ci.ksq];

        SERIALIZE(b);
  }

  return us == WHITE ? generate_all<WHITE, QUIET_CHECKS>(pos, mlist, ~pos.pieces(), &ci)
                     : generate_all<BLACK, QUIET_CHECKS>(pos, mlist, ~pos.pieces(), &ci);
}

/// generate<EVASIONS> generates all pseudo-legal check evasions when the side
/// to move is in check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<EVASIONS>(const Position& pos, ExtMove* mlist) {

  assert(pos.checkers());

  Color    us = pos.side_to_move();
  Square   ksq = pos.king_square(us), from = ksq /* For SERIALIZE */, checksq;
  Bitboard sliderAttacks;
  Bitboard knightAttacks;
  Bitboard cannonAttacks;
  Bitboard pawnAttacks;
  Bitboard b = pos.checkers();
  PieceType chkType1 = NO_PIECE_TYPE;
  PieceType chkType2 = NO_PIECE_TYPE;
  Square    chksq1, chksq2;
  int       checkersCnt = 0;

  assert(pos.checkers());

  // Find squares attacked by slider checkers, we will remove them from the king
  // evasions so to skip known illegal moves avoiding useless legality check later.
  //国际象棋只有slider check,中国象棋类型多
  do
  {
      checkersCnt++;
      checksq = pop_lsb(&b);

      assert(color_of(pos.piece_on(checksq)) == ~us);

	  if(checkersCnt == 1)
	  {
		  chkType1 = type_of(pos.piece_on(checksq));
		  chksq1 = checksq;
	  }
	  if(checkersCnt == 2)
	  {
		  chkType2 = type_of(pos.piece_on(checksq));
		  chksq2 = checksq;
	  }

      switch (type_of(pos.piece_on(checksq)))
      {
      case ROOK:   sliderAttacks |= PseudoAttacks[ROOK][checksq];   break;//意思是，如果rook将军，那么这个行都是被攻击的
	  case KNIGHT: knightAttacks |= knight_attacks_bb(checksq,pos.occupied); break;
	  case CANNON: cannonAttacks |= cannon_control_bb(checksq,pos.occupied,pos.occupied_rl90)|cannon_supper_pin_bb(checksq,pos.occupied,pos.occupied_rl90); break;
	  case PAWN:   pawnAttacks   |= pos.attacks_from<PAWN>(checksq, ~us); break;

      default:
          break;
      }
  } while (b);

  // Generate evasions for king, capture and non capture moves
  b = pos.attacks_from<KING>(ksq,us) & ~pos.pieces(us) & ~sliderAttacks & ~knightAttacks & ~cannonAttacks & ~pawnAttacks;//不能走到对手攻击的地方
  SERIALIZE(b);

  if (checkersCnt > 2)
      return mlist; // Double check, only a king move can save the day;中国象棋必须是3将，才能只能移动将

   // Generate blocking evasions or captures of the checking piece
   //Bitboard target = between_bb(checksq, ksq) | checksq;//中国象棋较复杂
  Bitboard target;
  if(checkersCnt == 1 )
  {
	  if(chkType1 == ROOK)
	  {
		  target |= between_bb(checksq, ksq) | checksq;//吃档,ROOK才需要档
	  }
	  else if(chkType1 == CANNON)
	  {
		  //排除炮架是炮的情况,
		  Bitboard mid = between_bb(checksq,ksq);
          Bitboard battery = mid&pos.occupied;
		  Bitboard t = (battery & pos.pieces(~us, CANNON) & (~SquareBB[checksq]));
		  if(!t)
		  {
			  target |= (mid|checksq) & ( ~battery);////档,要去除炮架，位棋盘处理起来的确很简单
		  }
		  else
		  {
			  target |= (between_bb(checksq, lsb(t)) | checksq) ;//两炮之间
		  }

		  //如果炮架是我方子，还可以移动这个炮架，但是不能放在target当中，
		  //这种情况需要单独处理

		  //还有种情况是炮架是我方子，改子不能沿着将军方向移动

		  //拆：只要炮架是我方子，并且只移动这个子
		  //档：移动的子不能是炮架
		  //吃：

		  //拆
		  //档
		  //吃

		  {
			  Bitboard forbid  = pos.cannon_forbid_bb(us);
			  Square sqbattery = lsb(battery);
			  for(PieceType Pt = PAWN; Pt <= ROOK; ++Pt)
			  {
				  const Square* pl = 0;
				  if(Pt == PAWN)  pl = pos.list<PAWN>(us);
				  else if(Pt == BISHOP)  pl = pos.list<BISHOP>(us);
				  else if(Pt == ADVISOR)  pl = pos.list<ADVISOR>(us);
				  else if(Pt == CANNON)  pl = pos.list<CANNON>(us);
				  else if(Pt == KNIGHT)  pl = pos.list<KNIGHT>(us);
				  else if(Pt == ROOK)  pl = pos.list<ROOK>(us);
				  

				  for (Square from = *pl; from != SQ_NONE; from = *++pl)
				  {

					  Bitboard b;

					  if( Pt == PAWN)
					  {
                          Bitboard att = pos.attacks_from<PAWN>(from, us) & ~pos.pieces(us);
						  if(sqbattery != from)
						  {  
							  b =  att & target;
						  }//吃或档
						  else
						  {  
							  b = att & SquareBB[checksq];//吃
                              b |= att & (~mid);//拆
						  }
					  }
					  else if( Pt == BISHOP)
					  {
						  Bitboard att = pos.attacks_from<BISHOP>(from, us) & ~pos.pieces(us);
						  if(sqbattery != from)
						  {
							  b =  att & target;
						  }
						  else
						  {
                              b = att & SquareBB[checksq];//吃
                              b |= att & (~mid);//拆
						  }
					  }
					  else if( Pt == ADVISOR)
					  {
						  Bitboard att = pos.attacks_from<ADVISOR>(from, us) & ~pos.pieces(us);
						  if(sqbattery != from)
						  {
							  b =  att & target;
						  }
						  else
						  {
							  b = att & SquareBB[checksq];//吃
							  b |= att & (~mid);//拆
						  }
					  }
					  else if(Pt == CANNON)
					  {
						  Bitboard natt = pos.attacks_from<ROOK>(from) & (~pos.pieces());//非吃子
                          Bitboard att =  pos.attacks_from<CANNON>(from) & pos.pieces(~us);
						  if(sqbattery != from)
						  {
							  b =  (natt|att) & target;//吃或档
						  }
						  else
						  {
							  b = att & SquareBB[checksq];//吃
							  b |= (att & (~mid))|(natt &(~mid));//拆
							 
						  }					  
					  }
					  else if(Pt == ROOK)
					  {
						  Bitboard att =  pos.attacks_from<ROOK>(from) & ~pos.pieces(us);
						  if(sqbattery != from)
						  {
							  b =  att & target;
						  }
						  else
						  {
							  b = att & SquareBB[checksq];//吃							  
							  b |= att & (~mid);//拆							 
						  }
					  }
					  else if(Pt == KNIGHT)
					  {
						  Bitboard att =  pos.attacks_from<KNIGHT>(from) & ~pos.pieces(us);
						  if(sqbattery != from)
						  {
							  b =  att & target;
						  }
						  else
						  {
							  b = att & SquareBB[checksq];//吃
							  b |= att & (~mid);//拆
						  }
					  }
                      
					  b &= ~forbid;
					  SERIALIZE(b);
				  }
			  }//for type
		  }

		  return mlist;
	  }
	  else if(chkType1 == KNIGHT)
	  {
		  //别马腿或把马吃掉
		  if( (ksq - checksq == 19) ||  (ksq - checksq == 11))//king在sq的S面
		     target |= SquareBB[(ksq-10)] | checksq;

		  else if( (ksq - checksq == 17) ||  (ksq - checksq == 7))
			  target |= SquareBB[(ksq-8)] | checksq;

		  else if( (ksq - checksq == -19) ||  (ksq - checksq == -11))
			  target |= SquareBB[(ksq+10)] | checksq;

		  else if( (ksq - checksq == -17) ||  (ksq - checksq == -7))
			  target |= SquareBB[(ksq+8)] | checksq;
	  }
	  else if(chkType1 == PAWN)
	  {
		  target |= SquareBB[checksq];//只能吃掉，这种情况除了移动将之外就是吃掉
	  }
  }
  else if(checkersCnt == 2)
  {
	  //只能是车马，车炮，炮兵，炮马，马兵， 马马等几种情况
	  if( ((chkType1 == ROOK) && (chkType2 == KNIGHT) ) || ((chkType1 == KNIGHT) && (chkType2 == ROOK ) ) )
	  {
		  //只能移动将
          return mlist;
	  }

	  if( ((chkType1 == ROOK) && (chkType2 == CANNON) ) || ((chkType1 == CANNON) && (chkType2 == ROOK ) ) )
	  {
		  //移动将或档
         if(chkType1 == ROOK)
		 {
			 target |= between_bb(chksq1, ksq) & cannon_control_bb(chksq2,pos.occupied,pos.occupied_rl90);
		 }
		 else if(chkType2 == ROOK)
		 {
			 target |= between_bb(chksq2, ksq) & cannon_control_bb(chksq1,pos.occupied,pos.occupied_rl90);
		 }
	  }

	  if( ((chkType1 == PAWN) && (chkType2 == CANNON) ) || ((chkType1 == CANNON) && (chkType2 == PAWN ) ) )
	  {
		  //兵正在将军，只能移动；可以吃兵,但这里不是移动将
		  return mlist;		  
          
	  }

	  if( ((chkType1 == KNIGHT) && (chkType2 == CANNON) ) || ((chkType1 == CANNON) && (chkType2 == KNIGHT ) ) )
	  {
		  ////只能移动
		  //return mlist;
		  //别马腿或把马吃掉,可能会

		  if(chkType1 == KNIGHT)
             checksq = chksq1;
		  else
		     checksq = chksq2;

		  if( (ksq - checksq == 19) ||  (ksq - checksq == 11))//king在sq的S面
		     target |= SquareBB[(ksq-10)] | checksq;

		  else if( (ksq - checksq == 17) ||  (ksq - checksq == 7))
			  target |= SquareBB[(ksq-8)] | checksq;

		  else if( (ksq - checksq == -19) ||  (ksq - checksq == -11))
			  target |= SquareBB[(ksq+10)] | checksq;

		  else if( (ksq - checksq == -17) ||  (ksq - checksq == -7))
			  target |= SquareBB[(ksq+8)] | checksq;
	  }

	  if( ((chkType1 == KNIGHT) && (chkType2 == PAWN) ) || ((chkType1 == PAWN) && (chkType2 == KNIGHT ) ) )
	  {
		  //只能移动
		  return mlist;
	  }

	  if( ((chkType1 == KNIGHT) && (chkType2 == KNIGHT) ) || ((chkType1 == KNIGHT) && (chkType2 == KNIGHT ) ) )
	  {

		  if( ((ksq - chksq1 == 19) &&  (ksq - chksq2 == 11)) || ((ksq - chksq2 == 19) && (ksq - chksq1 == 11)))
		     target |= SquareBB[(ksq-10)] ;

		  else if( ((ksq - chksq1 == 17) &&  (ksq - chksq2 == 7)) || ((ksq - chksq2 == 17) &&  (ksq - chksq1 == 7)))
			  target |= SquareBB[(ksq-8)] ;

		  else if( ((ksq - chksq1 == -19) &&  (ksq - chksq2 == -11)) || ((ksq - chksq2 == -19) &&  (ksq - chksq1 == -11)))
			  target |= SquareBB[(ksq+10)];

		  else if( ((ksq - chksq1 == -17) &&  (ksq - chksq2 == -7)) || ((ksq - chksq2 == -17) &&  (ksq - chksq1 == -7)))
			  target |= SquareBB[(ksq+8)];
		  else
		  {
			   return mlist;
		  }
	  }
  }
  

  return us == WHITE ? generate_all<WHITE, EVASIONS>(pos, mlist, target)
                     : generate_all<BLACK, EVASIONS>(pos, mlist, target);

}


/// generate<LEGAL> generates all the legal moves in the given position

template<>
ExtMove* generate<LEGAL>(const Position& pos, ExtMove* mlist)
{

  ExtMove *end, *cur = mlist;
  Bitboard pinned = pos.pinned_pieces();//要考虑不能走到炮架子上，要考虑对脸
  Square ksq = pos.king_square(pos.side_to_move());

  end = pos.checkers() ? generate<EVASIONS>(pos, mlist)
                       : generate<NON_EVASIONS>(pos, mlist);
  while (cur != end)
  {
	  //std::cout<<move_to_chinese(pos, cur->move)<<std::endl;
	  if (   (pinned || from_sq(cur->move) == ksq || (pos.checkers() & pos.pieces(~pos.side_to_move(),CANNON) ) )
          && !pos.pl_move_is_legal(cur->move, pinned))
	  {
		  cur->move = (--end)->move;
	  }
      else
	  {
		  cur++;
	  }
  }

  return end;
}

void test_move_gen( Position& pos)
{
	//Bitboard target;
	//target = ~target;

	std::cout<<"------------------"<<std::endl;

	ExtMove mlist[MAX_MOVES];
    ExtMove *cur, *last;
	cur = mlist;
	
	//last = generate_pawn_moves<WHITE, NON_EVASIONS>(pos, mlist, target, 0);
	// last = generate_moves<  ROOK, false>(pos, mlist, WHITE, target, 0);

	// last = generate_moves<  KNIGHT, false>(pos, mlist, WHITE, target, 0);
	//last = generate_moves<  CANNON, false>(pos, mlist, WHITE, target, 0);
	//last = generate_moves<  ADVISOR, false>(pos, mlist, WHITE, target, 0);
	//last = generate_moves<  BISHOP, false>(pos, mlist, WHITE, target, 0);
	///last = generate_moves<  KING, false>(pos, mlist, WHITE, target, 0);

	//last = generate<CAPTURES>(pos, mlist);
	//last = generate<QUIETS>(pos, mlist);
	//last = generate<NON_EVASIONS>(pos, mlist);
	last = generate<QUIET_CHECKS>(pos, mlist);

	std::cout<< std::endl;
	for(; cur != last; ++cur)
	{
		std::cout<< move_to_chinese(pos,cur->move).c_str();
	}
}
