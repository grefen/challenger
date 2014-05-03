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

/*
  The code in this file is based on the opening book code in PolyGlot
  by Fabien Letouzey. PolyGlot is available under the GNU General
  Public License, and can be downloaded from http://wbec-ridderkerk.nl
*/

#include <algorithm>
#include <cassert>
#include <iostream>

#include "book.h"
#include "misc.h"
#include "movegen.h"

using namespace std;

namespace {

  // A Polyglot book is a series of "entries" of 16 bytes. All integers are
  // stored in big-endian format, with highest byte first (regardless of size).
  // The entries are ordered according to the key in ascending order.
  struct Entry {
    uint64_t key;
    uint16_t move;
    uint16_t count;
    uint32_t learn;
  };

  // Random numbers from PolyGlot, used to compute book hash keys
  const union {
    Key PolyGlotRandoms[1351];//13*60+1//15*90 + 1
    struct {
      Key psq[14][90];  // [piece][square]
      Key castle[4];    // [castle right]
      Key enpassant[8]; // [file]
      Key turn;
    } Zobrist;
  } PG = {{
	      0x5D82CA6A2C19BCB3ULL, 0xA0F7F3FBFEF39367ULL, 0x6E125B00A0CD5A0FULL,  
		  0x1A1211C285AD06CAULL, 0x8E716BAA5250A383ULL, 0x968C5F3F59EA14EFULL, 
		  0xA8A7E1C2D3C62243ULL, 0xDD5DA4A13FFC75D2ULL, 0xABCD17B317EBB2C4ULL, 
		  0x424B38B414F844C7ULL, 0xCD99BD64E66A10C6ULL, 0x3B729A6E28A0210BULL, 
		  0xA6CCC0A7F1EEBF7FULL, 0x6F839E2A1BFFFC2BULL, 0xDEEAEBD8AAF454F8ULL, 
		  0x4ED3CAFE4BEB8389ULL, 0x6C6D8ED70CD2D066ULL, 0x8FD8AE140D2CC250ULL, 
		  0x308FBDFBD2AFB7E7ULL, 0x33576218DD56C075ULL, 0x5171280162E17EBAULL, 
		  0x4E947438630609A1ULL, 0xEBD616652CB8D83DULL, 0xB4758490436DAADEULL, 
		  0xA2F063C373134A59ULL, 0x8D8CDFBC7089118CULL, 0x568B80169B698384ULL, 
		  0xC327B8BE1FC3DA79ULL, 0x3E375B382AB4EDABULL, 0x4D428E815637210FULL, 
		  0x388132074DBE86C4ULL, 0x194E987F7A70DF2DULL, 0xDAFE23A05E92561FULL, 
		  0xDA7D12494C665560ULL, 0x7542E1EEBE8CCFAEULL, 0x7059230D036CFC82ULL, 
		  0xE6D25C349158E5EBULL, 0xD33728D4E801D5DFULL, 0xCBFC1EA9651BE951ULL, 
		  0x7BB52643E7253216ULL, 0x7E54E0FFBDCE5106ULL, 0x42BBC867E8ADC8C4ULL, 
		  0x4C2A98D5A853ACF7ULL, 0x96C9ED0EEA342C99ULL, 0x1D138D434AD619FFULL, 
		  0xB61EA560CD203BA8ULL, 0x7509E0169AD81006ULL, 0x243B39FAD286B40BULL, 
		  0x31F5B2AADDAF575EULL, 0xC0AD8AB9897986E9ULL, 0x2BC5BCB08C3169E5ULL, 
		  0x61B6C583605C81FBULL, 0xEE9D826F99294102ULL, 0x3F28B559C86F0275ULL, 
		  0x02D3812BC94FDE08ULL, 0x51E706751871C8FEULL, 0x2EF1BAEB93BAE27AULL, 
		  0xD3E075CFD449BB1EULL, 0xA23F7E2075FC0663ULL, 0x87E7FBECA8338670ULL, 
		  0x76294E6BD7BD1704ULL, 0x6F3744792F5FF73EULL, 0x7CE1998CBF4D202BULL, 
		  0xDEC84ECB89205640ULL, 0xE3D4D3FD47C5301CULL, 0xD32EBF8566811589ULL, 
		  0x3E262A8FD5D9AB0BULL, 0x33243FAAEE0EBAF4ULL, 0x5617B0BE86CB43BFULL, 
		  0xBF48BE23BF14387AULL, 0xC39064ABAA92D1D1ULL, 0xA3DEF306E12DFBBFULL, 
		  0x82C35E0ACD76BF5AULL, 0x17BF62F08FAE6AE7ULL, 0x0C387A540CC1F50AULL, 
		  0x218F3C999D6597F0ULL, 0xDD41A0F54F05FA22ULL, 0x20DF74AB28E31128ULL, 
		  0xCBB346498F03DDCDULL, 0xE51A6EDCC0923AD5ULL, 0x6FBE08D4AE0A1563ULL, 
		  0x108190CFFC6F9D39ULL, 0x8E2CD05D1B486946ULL, 0xDBF9E17010FD02E7ULL, 
		  0xA6031D2BD4E58206ULL, 0xB14210DEA726746DULL, 0x1B09EBE5B4CE852AULL, 
		  0x2F49FFF15935B41FULL, 0x457D4418B2F78026ULL, 0x27028B6CBEF5814CULL, 
		  0x80214F58611CAAE7ULL, 0x1135F6434E2902AEULL, 0xB70C655FF69348FEULL, 
		  0xA2E3F59F36FCF339ULL, 0x9B8337AD27184981ULL, 0x8AA140A505E6A3FBULL, 
		  0x2CAE82EF9465001FULL, 0x42C53661A515D15FULL, 0xF5C376BB0ABA165EULL, 
		  0xC9594CF8F2392E7BULL, 0x8328692BC6DD9746ULL, 0x6E4C5DCCFAA81A39ULL, 
		  0xBAFA39EC06204CE6ULL, 0xF2F28DE00E07F8D1ULL, 0xBDE1F9B7A6330ED0ULL, 
		  0x694D4A5390DDECA7ULL, 0xA463340CD911F56EULL, 0x36BEE5B851ACF98FULL, 
		  0x81B6C35577E44FC5ULL, 0x25A01BBF739E3C4BULL, 0xE9CCCBD3300B5E07ULL, 
		  0xCEEB14D716F492E3ULL, 0xDC042D484A1234B2ULL, 0x3AD76828F74C908EULL, 
		  0x8B4534023AD91EBDULL, 0x50422076C938252FULL, 0xA929FC40CBD94D58ULL, 
		  0xCD630ECDC5C3B218ULL, 0xC52C2B418FC4C11DULL, 0x1BD7BE722F6A1861ULL, 
		  0x58B850414270712BULL, 0xF803AD36F40E730AULL, 0xA9C11BB5F1863145ULL, 
		  0x3388EB57B6677C69ULL, 0xD33C53CB30F1C1DAULL, 0x95BACDC94B45709FULL, 
		  0xD21074367CD740D5ULL, 0x770CA513C7A4B558ULL, 0x0C651A1F22AF3373ULL, 
		  0xFF12042F0FED0E7AULL, 0x0318CEF63DE0C67FULL, 0x71E9453C13B78A37ULL, 
		  0xDDDC9BB377445C67ULL, 0xC436545C13992318ULL, 0x974D3F50530C44C8ULL, 
		  0xD47A10BB6C182714ULL, 0x3390F011B15DD250ULL, 0xC042FFD27E087455ULL, 
		  0xC4004F434933B5BDULL, 0xF10E5CACE7EFCF6BULL, 0x0533D44CF58022D8ULL, 
		  0xC5DE186CE0A2C04AULL, 0xA6E4C2F1EEDF1CCEULL, 0x651133694A59DD35ULL, 
		  0xB6C3CEF926AAF2E0ULL, 0x504B679B9800F058ULL, 0xCC9577FE4BE77AE4ULL, 
		  0x21283B78703B623EULL, 0x6F5D0CAF94A1D6C8ULL, 0x2029B24B5B16F67BULL, 
		  0xCD77EE5BC39BC2D1ULL, 0x504756D5C7E5FDC0ULL, 0xC5B829C492A5EA56ULL, 
		  0x3E39160A72C61709ULL, 0xACD2F3ED8C7BB30CULL, 0x3F1F716FCA036A2EULL, 
		  0xB8893848367CE5BFULL, 0x74B9E561376D337DULL, 0x22F33A25664EE05DULL, 
		  0x6954257C0413AFADULL, 0x9CF4A0052E21DEC5ULL, 0x390A9635000D6CD4ULL, 
		  0xF31A03B87BD0064CULL, 0xBA298FC9B6C2BFE6ULL, 0xD8CB9D0DD8C8BE1FULL, 
		  0x884E5B00A85D2449ULL, 0xE2EC5230B3079489ULL, 0xCDE281A2135B7B18ULL, 
		  0x81EE6675FCC2E715ULL, 0xAB2885084203D2F5ULL, 0xFF04919BEE40AB8BULL, 
		  0x82670F42E4ACFE41ULL, 0xF9EDD063AA3FB369ULL, 0xC8CDEF1A7BE618E9ULL, 
		  0xA8DBD192BFC98B5BULL, 0x8995BC144F86DD61ULL, 0xC4234AC269FB05C0ULL, 
		  0xADD7C4DA989C68F9ULL, 0xAEA5E6383D96055CULL, 0x3337175264B94B39ULL, 
		  0x7A042CDC9E504226ULL, 0x73FBA8185B7E3925ULL, 0x27000FDD046EF600ULL, 
		  0x7F51B6A279E17BFCULL, 0x4E4557C787B76A5EULL, 0x0D834EB3268D5E26ULL, 
		  0xC9DEA83640A90E38ULL, 0x0D17E5F0C4C93C78ULL, 0x8342E2B899B2CDB8ULL, 
		  0x83D605BB47557798ULL, 0x7CB8116EC8ADA475ULL, 0xAA241C70CF12993CULL, 
		  0x2B795712666B1E4BULL, 0x5DC9B4C93D76E4C9ULL, 0xBA822D56F73EB32CULL, 
		  0x94FF300897984482ULL, 0x497FA4BD32C16CB4ULL, 0xBCB13B2FDA17EB01ULL, 
		  0x6BC9C2B9413C1619ULL, 0x12117662B0BF333FULL, 0x83AAED30AA9F3B53ULL, 
		  0x9D83B2DB437E0081ULL, 0xC41966424624E36BULL, 0x57CA72404EB4F105ULL, 
		  0x00F1AC92ACD5D2D8ULL, 0x5802DE97953BFBBAULL, 0xD5B0443199CFE196ULL, 
		  0xF5F0B2973DA3AEE5ULL, 0xD0AF811D366DEC0AULL, 0x17B99CC7BEC69F23ULL, 
		  0x7B2A9685DDDC526EULL, 0xC40429A9D73D4BB2ULL, 0x347BF5D70AF7FF28ULL, 
		  0x20651B1992F31AADULL, 0x8448B109716CD3F4ULL, 0x6BC1DCD8CE593451ULL, 
		  0xC151C4AA88FA8F4DULL, 0xBB520847AF34B73AULL, 0x7B61F4AC696A7350ULL, 
		  0xE22FD72D8A1DFF86ULL, 0x080A4D49D4C44881ULL, 0x2D57F2198D4E3D32ULL, 
		  0x54545A2F1CE65168ULL, 0xBEE398ED55401431ULL, 0x8E9D324AF93A6466ULL, 
		  0xB1905EC59F072044ULL, 0xDA401C8DA18153B4ULL, 0xCF563F98A4D10219ULL, 
		  0x1DDE517FF61E6DC9ULL, 0x721C93D5835CC5B0ULL, 0x3DBECBF43DDB0CA6ULL, 
		  0x781876ADD402773AULL, 0x0A9487E2285FFC1EULL, 0x01200D8106C008CCULL, 
		  0xCC9C5845414D3503ULL, 0x228589B19E337377ULL, 0x6DAA66CF2CFE4507ULL, 
		  0xF1EBD1B6281D5995ULL, 0x7E86EEBD75ABD873ULL, 0xFABEE62C26AB4165ULL, 
		  0xE3DA53529C926F86ULL, 0x319CF420C08AC92AULL, 0x78C122F61653DCD6ULL, 
		  0xD6E1F60FFE91D63EULL, 0xF403996E5E21718CULL, 0x3C9D8623154DEF66ULL, 
		  0x6246AB82A37BA36EULL, 0x8F245F8AC6F3181BULL, 0x476F8209CD22B1C9ULL, 
		  0x24CDA63D091D1FB4ULL, 0x314B87A6407B7C47ULL, 0xFD4F624D7CCE50D8ULL, 
		  0x77152EF27558F188ULL, 0x093945CA4F61C0FBULL, 0x3CAF7670D5ED1619ULL, 
		  0xEFD9BF9FC09F3681ULL, 0xD09873AD2D13AC96ULL, 0xBD81F4E43E02CDCEULL, 
		  0x8F89F63059B9696BULL, 0x62949CC50B9E3929ULL, 0x902C18D15595E338ULL, 
		  0x14AC1AA6E89B07DFULL, 0x01A56ACE73C3720FULL, 0x435461C626D76AEDULL, 
		  0x6AC3A252AC7477C1ULL, 0x6D6AA50D224728A3ULL, 0x6CC3D235BE4D343EULL, 
		  0x73FB14BD66722CD8ULL, 0xFADA63F8BCF4BAE3ULL, 0xC4E113B20745D197ULL, 
		  0x2DDBD645F2D8A45DULL, 0x66078CFC76973C9EULL, 0x4B88DFFB9C0426DFULL, 
		  0xAF9E50537DDC4663ULL, 0x257AF35834F4F4CCULL, 0x3ADBC1DEF03D13BFULL, 
		  0x8EC4A3EE8E66A815ULL, 0xA0D14DFABCDD9F91ULL, 0x9CA5F2173F647B03ULL, 
		  0xCFC73A8349434DFEULL, 0x8B595C36BDA98837ULL, 0x8163522A1573EFD3ULL, 
		  0x6411D4C7C4C4E285ULL, 0x436224AE7C384C4BULL, 0xDD2BA267C9BD822EULL, 
		  0x34A3341415E9A7DFULL, 0xFF5F1DC90C8E2042ULL, 0x96A26583ABF0F210ULL, 
		  0x1B4BB6B1FC8F8ABAULL, 0xD5A6543CEBDCC629ULL, 0xF7E1C2E766319353ULL, 
		  0x9D3C89ABD26D9771ULL, 0x0A8FE41381B17881ULL, 0x50DF029FDB1926AEULL, 
		  0x833E808B936D11B8ULL, 0x9A773388ADE18890ULL, 0x11ADC35BF018B6EEULL, 
		  0x052C84692E1630F9ULL, 0x52259E49B8D53DE7ULL, 0x973080BCEEAC3E2CULL, 
		  0xFC4460CE8C90F283ULL, 0x3DEFF1E4E28219CCULL, 0x40278025484E580DULL, 
		  0x7BF59C7D9B4B271CULL, 0x3AF91A454B80AC9CULL, 0x868F42FD8EA386B8ULL, 
		  0x8DDD78A59AE4F152ULL, 0x03952E6A7525276BULL, 0x813729446A970F50ULL, 
		  0xB1839B5D831501C4ULL, 0x7A0823BB59E00B53ULL, 0x067DAF0A9AA4D12AULL, 
		  0x5A7B0BB2A2AFF85BULL, 0x3A4BDC98D1B38CF9ULL, 0x39856B0241770338ULL, 
		  0x6B739D3F05CAE042ULL, 0x845CCC3951AA0E03ULL, 0x5B9DED653490284BULL, 
		  0xEFFE72E182D542ADULL, 0xFFA72FA94CCCF9B9ULL, 0x2BC99DE58AA23092ULL, 
		  0xDD8329C4618728CAULL, 0x6A683872B26B029FULL, 0x2DF8865ACACBBE4DULL, 
		  0x15376DACDA2F0E81ULL, 0xD42D704592FC32B5ULL, 0x58FA4413070D3399ULL, 
		  0xB8C966145088CEADULL, 0xF668E74FA89E204DULL, 0x285ABAE848BBABE9ULL, 
		  0x2ADBCF8214392ED1ULL, 0x77F7C84D28329DC1ULL, 0xFA2405903D4B352CULL, 
		  0xA8AB3D31B7A9FAB0ULL, 0xA5535BFA41A91573ULL, 0x526AB53ECF3245C3ULL, 
		  0x5860F72591C7FFE2ULL, 0xF037CF2FFD87B278ULL, 0xCBD7A816A2BC2BC8ULL, 
		  0xE1D47F38AF50AD7BULL, 0xB373CDF32A493C2AULL, 0x6F30E922DABEA8A0ULL, 
		  0x62300FF2B414BFE8ULL, 0x6336411D3D35EEAFULL, 0xE15E767952EAFF63ULL, 
		  0x41596A60ED40C100ULL, 0x85FA09037AA9173EULL, 0x666DB06C94505F25ULL, 
		  0xC019C5D9CC58B557ULL, 0xCE1685F52A6CD919ULL, 0x4230F2C5694EE781ULL, 
		  0x2841082C5F3FA5B2ULL, 0x83F484CB8B58EA14ULL, 0xFC4DCC481F3367AEULL, 
		  0x06920C8FE1D1F022ULL, 0xABA9705D0D53F518ULL, 0x1F73D14BC7B2D94FULL, 
		  0x6006D68AC875E18CULL, 0x15538E2882949F20ULL, 0xE7A9D34A64F07716ULL, 
		  0x6F057A3D676715B4ULL, 0xBC4B79F8443DEB67ULL, 0xB7C4D38A8E286D44ULL, 
		  0x3422668CE3AC9E41ULL, 0x539446E33F9A2489ULL, 0xDA936278DAE94047ULL, 
		  0xD2993BDE5B9EFE02ULL, 0xD9843A58058CD05EULL, 0x6224E0B9DF6BBAE5ULL, 
		  0xD4C9935872C556F7ULL, 0x7109963FD7967FE7ULL, 0x40B8F3A9718DD495ULL, 
		  0x1BB4DB2814CCAE67ULL, 0x2F7B37B07BF78DCAULL, 0x0BA052BCE41C531DULL, 
		  0x55BB031322F81B81ULL, 0x413277827C6C895DULL, 0x3D3A0BDD3DBD7A6BULL, 
		  0xBFD6ADCD72CD7E38ULL, 0xB837FE068FF7466DULL, 0x58A5F58D0B3381F7ULL, 
		  0x6DDC0369F63CF094ULL, 0xC553019D39FEA9D8ULL, 0x6F0C43641723CB28ULL, 
		  0x7DC436965E127E9FULL, 0x98D0B57C9D1633E7ULL, 0xAB0BB22502751D21ULL, 
		  0xD4FAA0ADF2C33E63ULL, 0x71467A9DD43ECA0FULL, 0xCC6DA6AE21EE0895ULL, 
		  0xFF508A0B91B2491CULL, 0xDA445AB7D24FE053ULL, 0xC018539B031D2D50ULL, 
		  0xCC70894B0BBB3668ULL, 0xBBD791657371A8C2ULL, 0x0339202D3DD26F3DULL, 
		  0x9E46DA68E356FF9CULL, 0xBC8F4C48B2DC4C5BULL, 0xF26BD0B18AE35BEDULL, 
		  0xAF2CC40EC43BF203ULL, 0x0D776996B28E0B01ULL, 0xA3CC91D7955A8F75ULL, 
		  0xE4BD8F5314E4906EULL, 0x52367550A29B2B21ULL, 0x47E4F0D7A11ECB9FULL, 
		  0x085CED0D396813BFULL, 0xF9880B92D2B3A1E2ULL, 0xFA7E577F587E3205ULL, 
		  0x182E5981458D944EULL, 0xC23E809D53515C0EULL, 0xD332BD14CC488D2FULL, 
		  0xC508551E69624903ULL, 0x047BE5EF37163BA3ULL, 0x0D74B167E79C52E4ULL, 
		  0xAB99F16BFED0A5F9ULL, 0x1AF161057FAD49FFULL, 0x963E3E9BB0F149E7ULL, 
		  0xF030E3A10BAFCF29ULL, 0xBD65C2FF1AD7C38DULL, 0x1685A3505F7BD27EULL, 
		  0xD8EF4D779DD108E4ULL, 0xEA16509AB4A20DC2ULL, 0x4016DA2975718765ULL, 
		  0x11CB2708ED8E5151ULL, 0x2929E999E499E13DULL, 0xC4B76C6D372A5D9FULL, 
		  0xCC57FD9513711DD3ULL, 0x7A896B5A2978596AULL, 0x0D5520975BCA2D51ULL, 
		  0xA787CFA1172A875BULL, 0x4D54C002DDB0EB9EULL, 0x1B0A2B7B6AFB1C3CULL, 
		  0x1E310151A0215CABULL, 0x949A8C6E316BAE86ULL, 0x2689386CA9BC877CULL, 
		  0x3FE7416AB10C6D41ULL, 0xF1C25798145B2103ULL, 0x62E3966BFD77A38DULL, 
		  0xD284C6EF30BB1F9AULL, 0x408202E5171F6D90ULL, 0xC7CC502EA7CDAA6BULL, 
		  0x27C2C3BEBE070EE2ULL, 0x4E74C86E3D2BFD4FULL, 0x2FF999B0E3C00ACCULL, 
		  0xACD0324F38753836ULL, 0x9E45BA3EE7502CE8ULL, 0xC0D3EF3C4918E4B5ULL, 
		  0x4C2BC25F84C9500AULL, 0x0C60D645945FA0E3ULL, 0x4B7D333D34584BCEULL, 
		  0x50847113DC4DF626ULL, 0x7EDBA55C7DB5A85AULL, 0xF08AC6DD32AA79E2ULL, 
		  0x0FD07CB89770FCA4ULL, 0x1556026BF7988B0BULL, 0x53D158FB41D58AFDULL, 
		  0x3E6FC942816BCEC1ULL, 0xE463BB0B0AFBB623ULL, 0xC58AF9FE16449BB3ULL, 
		  0x050B020AABE94AB7ULL, 0x415A089418F75988ULL, 0x0C791EDD7BCF328AULL, 
		  0x5507A5965AB0E95CULL, 0x679B31FA63CB4AC9ULL, 0xBF361DFFD19E79B9ULL, 
		  0x086C1B02316D7734ULL, 0x79403C039766780AULL, 0xA91055C834DE8D97ULL, 
		  0x0B6683231E6D0087ULL, 0x17673477DF6F6320ULL, 0x52DFD153C6F4F180ULL, 
		  0x0A0245CF88CA639CULL, 0xD5A148E61E77F456ULL, 0x878E7D009E8E5975ULL, 
		  0xD8AB40ACBF1DC502ULL, 0x1F1ACE9DDEC15226ULL, 0xE573A526F83E6774ULL, 
		  0xCF9F0A31AAFFA79BULL, 0x1CAB005309EC91BCULL, 0xC52ED6F7EC24BA6EULL, 
		  0xF3C18763135FD806ULL, 0x2811F8917E885AF2ULL, 0x6518FCCA566B34CCULL, 
		  0xDB0EF166B295E69FULL, 0x83FE270F693A93A9ULL, 0x5771EADFFA304DBAULL, 
		  0x8A1AEF1F6E7429D0ULL, 0x1A1656B4ACE774B9ULL, 0x551E8B39AD57BB86ULL, 
		  0xF6A2FC48D8F6087BULL, 0xCA678CD5CA85C609ULL, 0x2FFEE5A912B938B1ULL, 
		  0xD504B08F3B70DEB5ULL, 0xB0BBF75434C5FA9AULL, 0xECEAFBC1D0C87FF6ULL, 
		  0x2032900113DEC7BBULL, 0x0B0FC2AFD52B7481ULL, 0x94D5A531738AE0FCULL, 
		  0xB2F02E18506110F1ULL, 0x25B836919BE51145ULL, 0xA92FF476E01D6A59ULL, 
		  0x502BD4BEA885F5C7ULL, 0x92DF42C2B7D8C223ULL, 0xB084F872A050C529ULL, 
		  0x82F47A63B7C7A82CULL, 0xB466F0EF9E7DD33CULL, 0xBE6FDE623DFFA1B4ULL, 
		  0x83C804F4DDB41969ULL, 0x20BA69D998830162ULL, 0x22BF46EE6FC7A7D6ULL, 
		  0x813FA1F5C6B0EC32ULL, 0x6FEF49E943CC6CA2ULL, 0x7F3F9C1A50E9B15CULL, 
		  0x424D73A79C95FC33ULL, 0xFE569330A71EE1D4ULL, 0x9CD515AD3483BD59ULL, 
		  0xB71920218FCEC3FDULL, 0x8425444D1568CCD8ULL, 0x583D1F364F55C365ULL, 
		  0x217538882ABDCF3AULL, 0x1775CA48D9542B6DULL, 0x468B4005393CC777ULL, 
		  0x72521E3DDE9ABF46ULL, 0x494B5406EC795304ULL, 0xA8E9EF50301DAAAEULL, 
		  0x641AF6DF8C39771EULL, 0x92960BEFD661DC51ULL, 0x07A2A036FDA72DF5ULL, 
		  0x719A2E57B28EB0FCULL, 0xDE11F44DACD41EBFULL, 0xC7B36774D31D24BBULL, 
		  0x16053F804CE7B077ULL, 0x7DAB584A43844024ULL, 0x4E92BAD920BE5935ULL, 
		  0x381ACFF270BCEB32ULL, 0xC7959C74E7B2ADD2ULL, 0xC6729EBF7698582EULL, 
		  0xEF67296241CCEF3DULL, 0x45B3C1DB103B6E04ULL, 0x70959A9738662BBFULL, 
		  0x5665B97727C8291AULL, 0xEA57D9725D6E63DBULL, 0xC53950F87EA9E017ULL, 
		  0x8FC507019C38AEFEULL, 0x6C36C3326773BC72ULL, 0x8E8503466CDC842DULL, 
		  0xC9E89C692CEE1723ULL, 0x591F640996C1BFB0ULL, 0xE52B4BBE6CE84FE3ULL, 
		  0x4FC28BBDB217236FULL, 0xF967AEAC0A4DCC62ULL, 0xA5C165C6CA848699ULL, 
		  0x7398BDE560CC0E4FULL, 0x359A88B828116D97ULL, 0xF5214B32399A950BULL, 
		  0x429881B851042899ULL, 0x580C1CFD5C0A7A17ULL, 0xA6F279BA4F3C60E5ULL, 
		  0xA2DA8F67F751BFF0ULL, 0x6B6BC9544983CCC3ULL, 0x61BBB8474D81E8C7ULL, 
		  0xAD3C72EE0F5833DDULL, 0x35BE82C817D900C9ULL, 0xC6D29B5E0115F8ECULL, 
		  0x4ED778194A57F87BULL, 0x8E62CB98A6C05C74ULL, 0x7D3331C19655A8BFULL, 
		  0xEFC3B26F22DECFF5ULL, 0xCF96A427AD400E08ULL, 0xBEAC4BF5C50C711FULL, 
		  0xCA3748263BEEC78AULL, 0x7B25B57007257DADULL, 0x70C1B894B14ECBDEULL, 
		  0xDF25487BA944BD4AULL, 0x729894D4CAF4EC45ULL, 0xD5097C694EC89480ULL, 
		  0x5AB654A2421890D4ULL, 0x478C17DE788FACE2ULL, 0x70F100E8B24932DCULL, 
		  0x154342A779EC1860ULL, 0xC735EEC6250B3583ULL, 0x8B515BA004347C1CULL, 
		  0x5D277F258C01D57BULL, 0x45001B997893B3C6ULL, 0xC4AD64853517160DULL, 
		  0x214863538D7FA052ULL, 0x94093661CF15CAD9ULL, 0x75C8F0C99CF247A8ULL, 
		  0x17A015AE5A1DE432ULL, 0xDD45875443D76FA5ULL, 0x823C0B79A0502FC7ULL, 
		  0x86437C240228239CULL, 0x88ABFDAEEC13E26FULL, 0xD307BC78C70082CCULL, 
		  0xAEE7DA615047A394ULL, 0xB929F1B7B34D4FFDULL, 0xF110E2E5A62C9528ULL, 
		  0xCFA3ECB0DE58C955ULL, 0xC28AB04C66101DFAULL, 0xD80DF8A3A34E2076ULL, 
		  0x9A9B31501CE1080BULL, 0x18AD11E289F01653ULL, 0x019DE1BA32F5F222ULL, 
		  0xBD3CAE1FC19E494AULL, 0xEA689BEE30021016ULL, 0x920915BB5D4892B5ULL, 
		  0x4A3080D061CC2FEBULL, 0xBE1F720FAAFD1861ULL, 0xBDD8FB2400C7524DULL, 
		  0x385B0253CD6039D8ULL, 0x05A0A95739C519A9ULL, 0x2BB94254527C6399ULL, 
		  0xEDECF2C4E3BFACB7ULL, 0x59AFFBE6F456E90DULL, 0x9CAE6471AD41ABADULL, 
		  0xAA9F7E5F501DACEAULL, 0x169A1E5EF4D3BF40ULL, 0x509A5E745AE7EBBFULL, 
		  0x7BF6A874BD0EBC5FULL, 0xDB39800FE3A1DB52ULL, 0x3672D0A12DAFDF3DULL, 
		  0xF4C02681A9DF67EBULL, 0x6AE5E3F6951EE2CEULL, 0xA7FF2E7E56D79DBAULL, 
		  0x4263FCBA59703FF6ULL, 0x7BAA6313CEB2A36BULL, 0x938E8D602F5DE404ULL, 
		  0x8041670E13481381ULL, 0x1F3126C9E2A63E12ULL, 0x1E4248DB8D8FB161ULL, 
		  0xD33EDB99DCA9564BULL, 0x4133162F55CFE571ULL, 0x8044DF8A70B8E0D4ULL, 
		  0x8D3F856E39651E9AULL, 0x16D098AA92089FB3ULL, 0x5FC4BB642245CC95ULL, 
		  0x6C6593379C925826ULL, 0x20B87718FA569EA8ULL, 0x86A68CF4F8183EFDULL, 
		  0x0FDB16EABB409D81ULL, 0x10CDF17A1DF5D326ULL, 0xE38BDCCD9E966C5BULL, 
		  0x7C6107D78E5A6D9CULL, 0xEF6E5C1F91F0A0BEULL, 0x154FA478336DFB12ULL, 
		  0xFE3311D5172DB5D9ULL, 0x3328D5AE6A6306CEULL, 0x5812E0245DDCCDDBULL, 
		  0x8F8B2EFA3463FAF5ULL, 0xA976ED94FC5407ECULL, 0xCBE9540649CE45DFULL, 
		  0x6AC3E2CB4815E792ULL, 0xCFCFC84EF73B4B66ULL, 0xC9FAFB5768F0C757ULL, 
		  0x81A0A36F689E3132ULL, 0x63008FED346D900CULL, 0x572C90E936CE0BCFULL, 
		  0xE0A0FFCE1BDAC21BULL, 0x1AC6AC5FC69C554DULL, 0x85DA499EE69C8ACDULL, 
		  0x1F914F0AA865E969ULL, 0xC9C2F1CD63A67710ULL, 0xD7245381E8695069ULL, 
		  0x6329C3A550AC7155ULL, 0x1FC25512B0A01CADULL, 0xD611D872B34DB42EULL, 
		  0x115781A62CD7EE27ULL, 0xA69473CEAF03156FULL, 0xE4CB69140A7FE983ULL, 
		  0xD3E07A9DB53872F6ULL, 0xA03FD9831172125BULL, 0xF55A9F75C4F947C5ULL, 
		  0x22048533D5056D75ULL, 0x3A754C78A46C73D9ULL, 0x3522A637F7615162ULL, 
		  0xB71CFF981E65BDE1ULL, 0x25078B9C5E94D615ULL, 0x5DA495198EFE3788ULL, 
		  0xD7C387E359360A7AULL, 0xBD9D75E37A6232ADULL, 0x80EAB13B84244E8DULL, 
		  0xB0AC26D0999C95A3ULL, 0x39D9E1D98A32328CULL, 0xAC763E36EDE6D193ULL, 
		  0xA66F44CF36DB3665ULL, 0x8748A05883A85B04ULL, 0x48DF12A6C89A732FULL, 
		  0x181DA563192A0D21ULL, 0xB9A5E990FCF45BABULL, 0xDB71016D34A7BA2EULL, 
		  0xF53DDC189BAC1A50ULL, 0x589E249B0A88CDF6ULL, 0x5A36972D73D7D8C8ULL, 
		  0x5ED8C53703F795A1ULL, 0x30BC6163B5461E07ULL, 0x16960C4C1F02C7FFULL, 
		  0x4CE3C590686BD538ULL, 0x044B1EEEDF17F715ULL, 0x5009AE644E650549ULL, 
		  0x3284FD2D5874707EULL, 0x4D7518F17263B177ULL, 0xE7964A962EBC3A79ULL, 
		  0x26A79E8EF958FFECULL, 0xF919DC2C020CAD6BULL, 0xCAAFDEF6DAB7DFA2ULL, 
		  0x638B19F5DCC099F0ULL, 0x34D0A854ACF35EF9ULL, 0xC3EC696700A83B34ULL, 
		  0x838C14AFCA632397ULL, 0x5A39789FB62D70A3ULL, 0x43E92F9F3204AF8FULL, 
		  0x4A1A852F47ED7F35ULL, 0xB760F61927B1BB04ULL, 0x368C7C002C6BFF36ULL, 
		  0x478C7758B9652124ULL, 0xA74A56429CC877FBULL, 0xCE836A1A353DCA4AULL, 
		  0x1FB0033BEE6D6C65ULL, 0xBEF3D73B36D1E59AULL, 0x8CD192A42A1FE1DAULL, 
		  0x4894281AB43E6DDAULL, 0xDF2B7AE4D4CA5217ULL, 0xD135B5B9F20DEE26ULL, 
		  0x58C002A801947B67ULL, 0x12D51A51211304E3ULL, 0xDC2EE29C8E17960BULL, 
		  0x092260B38FCD938DULL, 0x6944DDC43B6DEFB2ULL, 0x5A222E8D946BD9C8ULL, 
		  0x8BFEBE9B8E3A38D2ULL, 0xC4B355BD47565D3FULL, 0x83B9C03B1450F00BULL, 
		  0xAC604855CCC52F2FULL, 0x24B47E2898F496B8ULL, 0x4C5FEC57263D108AULL, 
		  0x946B5842771FC6B5ULL, 0x93AD95A2A25D01D2ULL, 0x20E88ED52D01A6CCULL, 
		  0x11000BC3D0AD55A4ULL, 0xD4BD5C0FBF4F48AFULL, 0xED203C5F35AEFE8EULL, 
		  0xD38204545FEED2AEULL, 0xFA3A46F757E8236BULL, 0xD9D91754B5811996ULL, 
		  0x3FDC4F8F5F70FD3BULL, 0x81A6236135E93729ULL, 0xF798CCEEDB21E2CAULL, 
		  0x351C673D4B4A6F22ULL, 0x14F8FF4D47DDC74CULL, 0x35AEAD7BB3ED5DE5ULL, 
		  0xB528A87EFC38DA9FULL, 0x9D45C550FB1B9B90ULL, 0xE775E80C8549CC5FULL, 
		  0xBC00E901C2C4256DULL, 0x05FCD11447AD5236ULL, 0xA6EE7B4DC21E6AD2ULL, 
		  0x8E0E519A8D9F6C23ULL, 0x92A5257E91CE7E72ULL, 0x8BEB73570D5BA8CAULL, 
		  0x5AC1721713028CC2ULL, 0xA6BE37501FCE663EULL, 0x57FC8603E8773970ULL, 
		  0x55D27C8D72024085ULL, 0x117EC7770F555F4AULL, 0x9DEB2E95B9ACB5DBULL, 
		  0xDE2046231AEFC0ECULL, 0x42BB07741267A1D9ULL, 0x89C7CBF6B7E4C177ULL, 
		  0xCF59A1C50C39DCF1ULL, 0x833FEABA027ED531ULL, 0xC1488C13F577613CULL, 
		  0x3A45E0D7D0AC2A01ULL, 0x3A7ECE948037341DULL, 0xD637A50FDBD7C52AULL, 
		  0x97CF8079A95A4BD4ULL, 0x8C44BBA75E5A9025ULL, 0x75FB5F44E11E505EULL, 
		  0xD5C16494D655B2ACULL, 0x2CE0028E7B712ADDULL, 0x5E1F39AA2994B08FULL, 
		  0x5A55641C0124C76EULL, 0xE45B46AD94EDA45EULL, 0x4AC403E6C3DA55B6ULL, 
		  0x4FF5EFAE52782500ULL, 0xA0A89BBEFE10EC4BULL, 0xBF6D796EBB02FF59ULL, 
		  0xCE83C4296F160FC7ULL, 0x224FEFFBB829778DULL, 0x724DA6DA361902FBULL, 
		  0xB1746B472CD84008ULL, 0xE83BA3B078D3913DULL, 0x5DAD6F66F63C004DULL, 
		  0x5AAD764A492B5D46ULL, 0xB10D73F767B2CE84ULL, 0xA7DEB18A445406E1ULL, 
		  0x0ED7E519193E7AD0ULL, 0x6886221E761A21C1ULL, 0x0AA326E363CCBAF0ULL, 
		  0xFFE76ACCBF2D5B32ULL, 0xE5EF01FC2A4B5A99ULL, 0x8F793DD2914EA0EAULL, 
		  0x3938F5BDA16CBF9FULL, 0x651E87BACE401CD9ULL, 0x536BA6D5B7B8656FULL, 
		  0xD396B37AFEE9F048ULL, 0x83FD5C26F60CFADEULL, 0xC7434F0F8A4B864EULL, 
		  0x0E1AA73C154FB86AULL, 0xD8D20DD86BBED531ULL, 0xAACA45D8BA54505AULL, 
		  0xB002DF369C50BCBEULL, 0x73D231C5FAD1B46BULL, 0xAA4C6C5989543E31ULL, 
		  0x9D38DD178E5F5599ULL, 0xBC759593F096E130ULL, 0xAE37035428D9C7A9ULL, 
		  0x44DC5F79EC641E5CULL, 0x600DAC33E9FD7A34ULL, 0x812EF3F78812D420ULL, 
		  0x2456CB3C9362D148ULL, 0x0D56E07F2F53CF67ULL, 0xA5A3404A86CE3875ULL, 
		  0xF5F2CB7F0250ADA9ULL, 0xACBBD8EB87511449ULL, 0xDF551A64A9CE28CDULL, 
		  0xA7FDB602FEE4AE2BULL, 0x2293B4795151A4F1ULL, 0xD05CA25E6DC12045ULL, 
		  0x47087FD52F0602AFULL, 0xE8ACBABA9CD62CEDULL, 0x3539193DEB19A4BDULL, 
		  0xFB96057185741BD2ULL, 0x03ABC8901CFC7E02ULL, 0xE38DDEE6DC21BE4FULL, 
		  0x536D194C39B9CDE7ULL, 0x1788F1A17D2D5A59ULL, 0x9E6726F8A3E5823BULL, 
		  0x820870B153B60E55ULL, 0x4630DC5F0013AC73ULL, 0x40736DF7C04116A2ULL, 
		  0xB6906C4DB67F610FULL, 0xD21F95613CA0EAF4ULL, 0x3AE20ED47D19E3DCULL, 
		  0x65E2421213AD1AECULL, 0x1BBC4E5613C4EDE1ULL, 0xB24F90D5C0E95205ULL, 
		  0x0C7FCEA7F2051756ULL, 0x30F892B02C49ABB7ULL, 0x0F453072D940623BULL, 
		  0xB2077D82AE6CA2D8ULL, 0x911C2BB99E2C0641ULL, 0x68E315A26EFD5C36ULL, 
		  0x6A3B0EA0FF318B95ULL, 0xA3B972D2D6FDF7A4ULL, 0x599D2C5A69F9ACBCULL, 
		  0x5C9E1DA2F6FA9BCCULL, 0x3DECD74E01414940ULL, 0x224BA4706B6E4B4CULL, 
		  0x79B29C72AE77D94BULL, 0xF563875C8695F507ULL, 0x4A6CFA80663A3A11ULL, 
		  0x9933C03766310656ULL, 0xC809D91E38D6D687ULL, 0x68DC9FBA2B39E42CULL, 
		  0x0897A7BE56B1C09EULL, 0x491A2A0EF97D86EAULL, 0x5FEAA2AD34720A8AULL, 
		  0xA2EB09D4481B0CF9ULL, 0x7D934D8A1A091D39ULL, 0xE2846BC41049330DULL, 
		  0x39FBB678CE3E2038ULL, 0x27C0D33D57F45B13ULL, 0xBFF887C50C90C6A5ULL, 
		  0x730B873DA2C01B49ULL, 0xD9842207CA1245BEULL, 0x8308C92883AD48A8ULL, 
		  0x92A58F45A4744924ULL, 0x2DB3B9BFB6BB756DULL, 0xA71FCA60B492C634ULL, 
		  0x7681064A422D9BC4ULL, 0x78ED3A45DC3A2283ULL, 0x00ACC385C8A51A78ULL, 
		  0x8A05F9E270C08F5BULL, 0x798951DFBBAAD71DULL, 0xC9E31A6AC26B7158ULL, 
		  0x0339A47DFB718DAFULL, 0x3BD54AFABB5A476DULL, 0x0EE4F85BB4611AC6ULL, 
		  0x398CFDA3BD42E866ULL, 0x50AEB36E95076C39ULL, 0xFED0CE62C80F766CULL, 
		  0x03DD4CE0C73A9F1EULL, 0xEDF029E1E6820997ULL, 0x7BED7BD01E541ABFULL, 
		  0x1E9224361A96AA8EULL, 0x6AE76AC7AF2F9510ULL, 0x3D95717B07EA4022ULL, 
		  0x1C5CDD81D810F165ULL, 0x9170D4DDEDC4A027ULL, 0x5BF0CBE80BB3C76AULL, 
		  0x0E38295E540E8F3EULL, 0x6B38CF852BA03FE0ULL, 0xB21EB841DE31B8C0ULL, 
		  0xA554C79B8071361FULL, 0x5241F4BF8A2D6C87ULL, 0x6516BF6C72D1FCC1ULL, 
		  0xA5F98F59C0FF2CAEULL, 0xAF38ADB2F62FF308ULL, 0x4F3713E5C2799227ULL, 
		  0xD56783B879556C78ULL, 0x88BC1AAC1B6612ECULL, 0xE536B758C19BE96AULL, 
		  0x42EEA3D4B280224FULL, 0xED8AA4826EB8A36FULL, 0x8EC960BD39D6B2A6ULL, 
		  0x4EDD4D07F1B096C6ULL, 0x921F134BD424E60CULL, 0xC662C2C23035133DULL, 
		  0x48EBCF01572A6510ULL, 0xE44491B969894CD7ULL, 0x86DD7EBC4502511CULL, 
		  0x56C9A56698914AA1ULL, 0xFD8B353773E85782ULL, 0x29B648F4C88DB1C9ULL, 
		  0xECFC42A8D6494EEAULL, 0x6715EB34A9C24A5AULL, 0xDD869217BDCBF2A1ULL, 
		  0x6F86683604A0EDF6ULL, 0xA9EB8976C38C4A9EULL, 0x773F62680646A328ULL, 
		  0xA849B4175CA54C9DULL, 0x97E1C617737A8ACCULL, 0x20F9740C2EC882EBULL, 
		  0x110A6CF8C7D03437ULL, 0x5E53CC9D4312E29DULL, 0x3F886FACB2B12269ULL, 
		  0xE8123240047DA329ULL, 0xB09D603AE2463254ULL, 0x67CB65714007D6B2ULL, 
		  0x7DC093E942865707ULL, 0xBBA9ECF8D4D1A3BDULL, 0x7FFF2FA7A7EE4233ULL, 
		  0xEB02FDDD8FE3B10CULL, 0x38A204368385FD8FULL, 0xEDEC504C3305B626ULL, 
		  0x9B00D12355A32F86ULL, 0x96B498EC38CCFD8EULL, 0xB7CB8F074CC08EFEULL, 
		  0xCE01D278B1E1D9E5ULL, 0x37FBDFDBD43AAC36ULL, 0xACE911C8F334A824ULL, 
		  0x0D934F66C1061983ULL, 0x20FAEC63A955CE89ULL, 0x39E1BD6E83CAC75DULL, 
		  0x69496D012AAB2BEEULL, 0x79CCCCBCF11E90BDULL, 0x6772973494F1BAC4ULL, 
		  0x505B1B6188ED8532ULL, 0x512A8A12229037CEULL, 0xF4B1F12A5B142892ULL, 
		  0x5A1047041D26F18DULL, 0x9213DF13FEB41FB0ULL, 0x41A694844C7751E5ULL, 
		  0x44D5DDA1C61E7400ULL, 0x5C24192620047EE6ULL, 0x51311D4C29D0A878ULL, 
		  0xA02E020FFCDBF703ULL, 0xEA03DD8BDA4AEA3CULL, 0x63E1938395D9E8F5ULL, 
		  0xB897ED785B400670ULL, 0xA2D241EB5F0F697CULL, 0xB5A3B7899FA0154FULL, 
		  0x7CC33E1FEDF67830ULL, 0xF629BE34BC4661A5ULL, 0x3651A6657758F5B8ULL, 
		  0xD5A1063610CD8535ULL, 0x13E3DDAADB03B419ULL, 0x70BDF28471A42D79ULL, 
		  0x59324534AC34A130ULL, 0x2B3AFD293868D2ADULL, 0x86D09A512BCB7BB6ULL, 
		  0x3F1694A93E617E64ULL, 0x8212538FD52FC719ULL, 0x22E7134A4257A928ULL, 
		  0x6C501A521CC71839ULL, 0x87B5D48371E7E3AFULL, 0x9C1296BD90357CD4ULL, 
		  0x7BD2793C60E532D2ULL, 0x52DFD310771B4DCAULL, 0xA32DB0ACFF200017ULL, 
		  0x70564E225C51DAD4ULL, 0x2BBA3F2A01C3B91EULL, 0xB8FFCE87EB61086AULL, 
		  0xB8E92C0BAEC2286DULL, 0xD0CBE6AA01A006ACULL, 0x5DE330ED581B40F8ULL, 
		  0x00DD3D6457179CFDULL, 0x47ADEAC71ECB8795ULL, 0x565DA36CEF95685BULL, 
		  0x19C558F9C691337FULL, 0x1E60EF4DA45B0032ULL, 0x5F41EC3618FB84A7ULL, 
		  0xD8937A2271F91167ULL, 0xA98C5B9D29DE98A3ULL, 0x54BBA6A82FF161BEULL, 
		  0xEFCD408A453F0952ULL, 0x0BFB43C32CED4EDFULL, 0xB2FDAEBA3E9AFCCAULL, 
		  0xE34765BDF779A171ULL, 0x57C5AF9407A2B16CULL, 0x2A46B1614D4A751EULL, 
		  0xE1B2FC7551C9ABDFULL, 0xE6228F6525BD066DULL, 0x6B06E19660C9A424ULL, 
		  0x4CBFEB46732A145FULL, 0xCF3E76181B3B366BULL, 0x151B2E9E4A462A74ULL, 
		  0xCCAAB247F4BDE9DDULL, 0xD847135774C745B7ULL, 0x916B572CA83F60CBULL, 
		  0xCD321AC0AB1B27DBULL, 0xE5BAFFB74FC3578BULL, 0xFC71E078565EA590ULL, 
		  0x61E095EE094D89C0ULL, 0x5D9ACCBB11849775ULL, 0xAD28E9724246FB6DULL, 
		  0x94F6357C5BF54F6EULL, 0xBB96FA909D744BBEULL, 0x7A01F6036E697475ULL, 
		  0xBD0F6CB9EFE7DF48ULL, 0x1FD40A6B152EDC9CULL, 0xD5A51CC7C8CC4F03ULL, 
		  0x4617ADC790FCBDA7ULL, 0x9C97884E671DFB2DULL, 0x3B727C091687614EULL, 
		  0x3DCD8604253391EEULL, 0x8ADBF245D5707441ULL, 0xAFD2C1F869662217ULL, 
		  0xB84073B30FB2808BULL, 0x61D7034EB5C254C1ULL, 0x0FCE70C7A3A42D72ULL, 
		  0x30F9A91E76FE79B0ULL, 0xDD5A61DA47EC0B14ULL, 0x67E60432CE71DDAFULL, 
		  0x02275E35584D2484ULL, 0x62B786C05BC0400BULL, 0x484FC52E48ED8221ULL, 
		  0x2588ECB564678A5BULL, 0x7751C308E219596DULL, 0x5010B816944FC1B7ULL, 
		  0xD7076F1B18BC5511ULL, 0x951AE876CE794EAFULL, 0xFABA3A0D8CD1F0BCULL, 
		  0x9B7F3D26C3328482ULL, 0x499FD84044E6997AULL, 0x0E6901074251D6B6ULL, 
		  0xCA0D846AE9A11286ULL, 0x0E3B173194B8DF44ULL, 0x8C061B1BB3620F8EULL, 
		  0xBDDBDC0429CEDD38ULL, 0xCDA5B1C1E8DDE415ULL, 0xA1BAF7F068C3E168ULL, 
		  0xF6222898B1C6E9A5ULL, 0x98B71068CDB4C3FFULL, 0xB644C87A637049FFULL, 
		  0x5AC79AD3834ED3CAULL, 0x7B1FA4B70D5A6D77ULL, 0x0B46FEB8DE8C286FULL, 
		  0x4C74269C4C41DBD9ULL, 0x2B08B55A0B275896ULL, 0x72C6D76BE7229AEFULL, 
		  0x44DEBCB61F1B3F7EULL, 0x2C01E709AEFFE4F6ULL, 0x102F789F7A09D4FEULL, 
		  0x811DAF9DB29A5514ULL, 0xE3F6CCED425D6DA8ULL, 0x4F82AB402E9A4EDDULL, 
		  0x726A6190BBB2C2CAULL, 0x0A3A2F4FDF412715ULL, 0xC626E2723095A9FEULL, 
		  0x88A421B60ED80C04ULL, 0x1996F5093358FEC3ULL, 0x2A558E5216700356ULL, 
		  0x2512E46A78FFE668ULL, 0x4EEE109968C28007ULL, 0x910838B85330BD04ULL, 
		  0x16443048A33E554BULL, 0xF4C42266C44BDD83ULL, 0x20D48AFF0FC03A8DULL, 
		  0x87E949929D7EF8D4ULL, 0xEAE2696938C614F5ULL, 0x25C08F5E4009A731ULL, 
		  0x820011837BFC2E30ULL, 0xFEA6F3B3DDDCE3B4ULL, 0x1D7707DD51D9B7C1ULL, 
		  0xFE87161255818FC2ULL, 0x1FE4BA0010B09053ULL, 0x6D00964A5831F0C3ULL, 
		  0xA17811FF960269CFULL, 0x1088DDDDFEAA611FULL, 0xE356101AEE60CAA2ULL, 
		  0x226D78E7DD83F908ULL, 0xB9DBAB4E00B4FAABULL, 0x0F9CB49457646AA5ULL, 
		  0x6A39DEAF9873056FULL, 0xEB130F7FD2FD4475ULL, 0xD7F0AD224BA3E09BULL, 
		  0x6F399F6E97BDCF17ULL, 0x457C8440000A4FBEULL, 0xB094C5EFAEC5AAEAULL, 
		  0x4AD8482C7A6E87C8ULL, 0xEF5C92378D24F291ULL, 0x74DC7E0021F04FF4ULL, 
		  0xE4D304899AB5A15AULL, 0x4B4E35DE262CF307ULL, 0xB4B39EE4324BF4DCULL, 
		  0x8894E75B43C7492BULL, 0x913F092AD7B8DDC1ULL, 0x87A19099D878AF1CULL, 
		  0x271ABC47D0689ED6ULL, 0x7D3E4500125CF30BULL, 0x51224E33DF4CC37AULL, 
		  0x4B71694A0DAAD435ULL, 0x163A9C9DD10B1381ULL, 0x1D3AA0291C899293ULL, 
		  0xEB2319909C2CBC62ULL, 0x1F41CC385B381F85ULL, 0xF7D01F64876F8446ULL, 
		  0xE282CEC966DD2343ULL, 0xC7CDF5582C6C6A0CULL, 0x3B128E3D33DA2702ULL, 
		  0x49492FB8255C9D06ULL, 0x6746E5A17F58B3E3ULL, 0x523F4209F26E3071ULL, 
		  0x6947A4084C916579ULL, 0x1C6A7F7A88288D3AULL, 0x68A8D531820AFB90ULL, 
		  0x386D98BBC8E1CB6EULL, 0x9B820593DE89F3F4ULL, 0x28FE1B7F155B520DULL, 
		  0x831681511359630DULL, 0xA2E6AF2AECCDC4F1ULL, 0x8BB5C57749F730C3ULL, 
		  0x5AADAB203DDDD939ULL, 0xE590C219A6634C88ULL, 0x6EC55DF66B34C4F0ULL, 
		  0x9A470566A1F5900DULL, 0x224967589BAC6C32ULL, 0x9D13EC3DC98F9E15ULL, 
		  0x798B274F77F170E9ULL, 0x59573E4D7757DE29ULL, 0xAF2BC573237027DEULL, 
		  0x4B37860B81296505ULL, 0xADF97E687817F463ULL, 0xC63A07F0DACAB254ULL, 
		  0x12434BA6A170C3CAULL, 0xF4F11EC116239925ULL, 0xDA9C22A1861793A7ULL, 
		  0x3C3B4AC1E19996CDULL, 0x01EE88826E6F8AC3ULL, 0x1849CEEE1708023DULL, 
		  0x18ADBBC7D9A67CB1ULL, 0x1C74C832DB477134ULL, 0xB6D6ABBF8EB5D94EULL, 
		  0x290D4B419A6B54DFULL, 0x5B5C144C84ED14F6ULL, 0xA27CFBC45060CFFFULL, 
		  0x1456D9430A202E70ULL, 0x9E9FF002C1BC210BULL, 0x4292482C780487EEULL, 
		  0x9BA90E9669989270ULL, 0x66A4093303873F83ULL, 0x3B86307995565F94ULL, 
		  0x9029F365110BE290ULL, 0xF7A3D17371517570ULL, 0xAAAB9DD113995B8BULL, 
		  0xFC3980E5BDA88774ULL, 0x82C1394DB4FF52E5ULL, 0xA960E3635DA84837ULL, 
		  0x55F03B9E4CC8CC2AULL, 0xF4292618CC4BB5CAULL, 0xC13ACD31C0C6CE3EULL, 
		  0x59A93D2A96264AE3ULL, 0x3A526A0CF9EE947AULL, 0xF7706250E86A7F8BULL, 
		  0x4F01006FCF6E149FULL, 0xC7E837B59DAB20FCULL, 0xDE1AD3ADC8B5F0EBULL, 
		  0xB51953811BE8B8EFULL, 0x2F18D38EB36C4327ULL, 0xDE24FDA3BB65A309ULL, 
		  0xFD5905DAFDE9ED84ULL, 0xDA92FA9A170257A6ULL, 0x53B6B0CABCC30DB4ULL, 
		  0x7DCA2542FDE6A808ULL, 0x677C76065EA5AD3EULL, 0x29A384756296EBC6ULL, 
		  0x233244D5D0DF2D14ULL, 0x2395ED923F2B6A20ULL, 0x3B050E665C75A86DULL, 
		  0xC433A8733B4EF6D6ULL, 0xC4C08489C7FBA97DULL, 0x6E555857D3509BB6ULL, 
		  0x2BB0539B35A44660ULL, 0xB4B99EF00D5470B4ULL, 0x9B37C43C00B7A271ULL, 
		  0x00B7FDBEDFF45A94ULL, 0xCAAA63D9668BB548ULL, 0x41D6DFB8FCE05348ULL, 
		  0x2A126BCFA027630EULL, 0x9DC19819A21C11A2ULL, 0x4250DBC6DAA05E4FULL, 
		  0x87E0FB60A8341B3DULL, 0xB99D0C8FC8C348E1ULL, 0xBD7EE5E5E5841A26ULL, 
		  0xAEA477104AD3A4CCULL, 0x993703486C37DDBFULL, 0x2F282B758B463BBAULL, 
		  0x40346D38B9A38263ULL, 0x949BB1ECFE187855ULL, 0xAD9FB8648E3E0BD6ULL, 
		  0x2207BB529BDCBC89ULL, 0x21D0B668550D6F5FULL, 0x7F12439F846A4CF8ULL, 
		  0x8302845F65665E3FULL, 0x1F35D285B6297771ULL, 0x76A0B0E4FE697660ULL, 
		  0xFA93F1E7D7141084ULL, 0xAB9D37D76D182DE5ULL, 0x9E5736B1AB409B7CULL, 
		  0x3B5A8B8410108CA9ULL, 0xA61D37F811D5FDF3ULL, 0xEAFC7FF4AADA78C7ULL, 
		  0x6C30B4D34D691A1EULL, 0xF00F8A27E8EA1788ULL, 0xEFAAB7C3FD1FE5AAULL, 
		  0x770249D0A77EFD3BULL, 0x810D87BB30D13B09ULL, 0xEECE7E8165CBC62DULL, 
		  0xBB2FA83357036A61ULL, 0xF23473046FE64A73ULL, 0x3D52457616E627F5ULL, 
		  0xC8445FCB404E9669ULL, 0xC682E47C42E5F5B2ULL, 0xB17A3785450AA2B1ULL, 
		  0x01A85E6FD2D0B8F5ULL, 0x0D645419E85F9034ULL, 0xB14A51E390598DE2ULL, 
		  0x7F141B1F9E7CE0BEULL, 0x27ABE9C2B67D1F6EULL, 0xB483DB1DF63212F8ULL, 
		  0x29052E2C89474DF2ULL, 0x27D493AD112A1057ULL, 0xA39AC5CB0BA56D11ULL, 
		  0x09DFD822010934DCULL, 0x43871AFA5527AA79ULL, 0xECE72F10C169DD25ULL, 
		  0x748B4FB2D1543DF1ULL, 0xF9C5D7FA50020EEEULL, 0x4052BA90D27F5FB3ULL, 
		  0xDC6E3391BDCE3658ULL, 0xE91BF6C642620791ULL, 0x5764A7DC2CE3856AULL, 
		  0x63DAD70F5FC20DBDULL, 0x1EC14495655C4949ULL, 0xCA105D0795A465F5ULL, 
		  0x1F0987744B6C851EULL, 0xC19BCDF7725F5E42ULL, 0xACD9F6B3A7C739EFULL, 
		  0x0C0EFD427DA37A5FULL, 0x551AEBE6E6B24211ULL, 0x870DEC3C1B531487ULL, 
		  0xE570A6082158F34CULL, 0x9403BC029846FA86ULL, 0x779AB3CCDAA0B866ULL, 
		  0x8F3F87FB32415720ULL, 0x68C1095B29B1521BULL, 0x219EF1A8FA5ACA58ULL, 
		  0x319ED20E6B3DC9E3ULL, 0x44003B56001545BEULL, 0x9D604433C05F3A75ULL, 
		  0x8304AC77D422C622ULL, 0x994D272D22189952ULL, 0xAD60CB2F2B4A238FULL, 
		  0xB7B2A4157E4A623EULL, 0xFC0F3F922C37B57FULL, 0xE5DF3C0B746F9802ULL, 
		  0xACCFA469493B6A31ULL, 0x57B8D485125180C0ULL, 0x41B6D355F01C2E6EULL, 
		  0x7F5177876F94A360ULL, 0xE9154801A2EBCBA6ULL, 0x2DAD297A821C5E61ULL, 
		  0x51E13E84DC1FCDD5ULL, 0x2F7B655ADAB2B4A1ULL, 0xBBFAB747E579548EULL, 
		  0x01C87B08CFF50D3CULL, 0x9886C7F2C74EF095ULL, 0x18AE9DEF50AB83F9ULL, 
		  0x73C82DEB1089DADDULL, 0xBB8DB619FD613A36ULL, 0xF6CFF4CF283AD42FULL, 
		  0x9A903AC7548D3039ULL, 0xC4A53EB07A9E4273ULL, 0x4A05D20A56E64F62ULL, 
		  0x0301D35AEC98DE1BULL, 0x9975C2EEF1AF238BULL, 0x6CC3C8C7A4523C63ULL, 
		  0x8F7C74603C54A732ULL, 0xDDB8C2A6445B1D85ULL, 0x2FEA3EFB8693E85BULL, 
		  0xDE8BD8ED2632FC8BULL, 0x1756A452AF3AD798ULL, 0x7170D576542A5C97ULL, 
		  0x89E3B1536ED4D2D3ULL, 0xF21AE4E8B7656A70ULL, 0xECF5A2D368FE6261ULL, 
		  0x224DE1BFED2D174EULL, 0xA33E07AE10C62D4AULL, 0x06102E5F6AF9FA3BULL, 
		  0xA8566927E4908AD6ULL, 0x94EE734291472250ULL, 0x74E4D2E9B023016CULL, 
		  0xFFEDE299ED558029ULL, 0x73405527C1E93E3DULL, 0x9CA4DB038D7EEB11ULL, 
		  0x512FEAA28F347E29ULL, 0xF57F6CD07B9B247CULL, 0x276F9733A9813BC9ULL, 
		  0xAD050BB855AC034BULL, 0xF77C5A4F06F2E497ULL, 0x04953DB42F94833CULL, 
		  0x2148FD8674194B77ULL, 0xA684CF39FF1D5A41ULL, 0x4131C3246A36063CULL, 
		  0x21DCAA82FB3D54F4ULL, 0xCA5AEED84CCC09AEULL, 0x54FB4DB07456C270ULL, 
		  0xD12DBBFACBF6BB59ULL, 0xF96AF2F3E1D02B54ULL, 0x70DDF5FFD453E2C7ULL, 
		  0x7380F75287996FAEULL, 0x4C4E7FB35319FCABULL, 0x64D6DC20E1A84ACBULL, 
		  0x161DB07C22BF2658ULL, 0x1D3584C0610AB3D8ULL, 0xD98F9CFA6E2DADD2ULL, 
		  0x366FD8DBEAEB262FULL, 0x8FFA6F0DAEEEF7FBULL, 0xE4AF5DCA9E19758DULL, 
		  0xB60F5777A07FD280ULL
  }};

  // polyglot_key() returns the PolyGlot hash key of the given position
  Key polyglot_key(const Position& pos) {

    Key key = 0;
    Bitboard b = pos.pieces();

    while (b)
    {
        Square s = pop_lsb(&b);
        Piece p = pos.piece_on(s);

        // PolyGlot pieces are: BP = 0, WP = 1, BN = 2, ... BK = 10, WK = 11
		//BPAWN,WPAWN,BBISHOP,WBISHOP, BADVISOR, WADVISOR, BKNIGHT,WKNIGHT, BCANNON,WCANNON, BROOK,WROOK, BKING,WKING
        key ^= PG.Zobrist.psq[2 * (type_of(p) - 1) + (color_of(p) == WHITE)][s];
    }

    //b = pos.can_castle(ALL_CASTLES);

    //while (b)
    //    key ^= PG.Zobrist.castle[pop_lsb(&b)];

    //if (pos.ep_square() != SQ_NONE)
    //    key ^= PG.Zobrist.enpassant[file_of(pos.ep_square())];

    if (pos.side_to_move() == WHITE)
        key ^= PG.Zobrist.turn;

    return key;
  }

} // namespace

PolyglotBook::PolyglotBook() : rkiss(Time::now() % 10000) {}

PolyglotBook::~PolyglotBook() { if (is_open()) close(); }


/// operator>>() reads sizeof(T) chars from the file's binary byte stream and
/// converts them in a number of type T. A Polyglot book stores numbers in
/// big-endian format.

template<typename T> PolyglotBook& PolyglotBook::operator>>(T& n) {

  n = 0;
  for (size_t i = 0; i < sizeof(T); ++i)
      n = T((n << 8) + ifstream::get());

  return *this;
}

template<> PolyglotBook& PolyglotBook::operator>>(Entry& e) {
  return *this >> e.key >> e.move >> e.count >> e.learn;
}


/// open() tries to open a book file with the given name after closing any
/// exsisting one.

bool PolyglotBook::open(const char* fName) {

  if (is_open()) // Cannot close an already closed file
      close();

  ifstream::open(fName, ifstream::in | ifstream::binary);

  fileName = is_open() ? fName : "";
  ifstream::clear(); // Reset any error flag to allow retry ifstream::open()
  return !fileName.empty();
}


/// probe() tries to find a book move for the given position. If no move is
/// found returns MOVE_NONE. If pickBest is true returns always the highest
/// rated move, otherwise randomly chooses one, based on the move score.

Move PolyglotBook::probe(const Position& pos, const string& fName, bool pickBest) {

  if (fileName != fName && !open(fName.c_str()))
      return MOVE_NONE;

  Entry e;
  uint16_t best = 0;
  unsigned sum = 0;
  Move move = MOVE_NONE;
  Key key = polyglot_key(pos);

  seekg(find_first(key) * sizeof(Entry), ios_base::beg);

  while (*this >> e, e.key == key && good())
  {
      best = max(best, e.count);
      sum += e.count;

      // Choose book move according to its score. If a move has a very
      // high score it has higher probability to be choosen than a move
      // with lower score. Note that first entry is always chosen.
      if (   (sum && rkiss.rand<unsigned>() % sum < e.count)
          || (pickBest && e.count == best))
          move = Move(e.move);
  }

  if (!move)
      return MOVE_NONE;

  // A PolyGlot book move is encoded as follows:
  //
  // bit  0- 5: destination square (from 0 to 63)
  // bit  6-11: origin square (from 0 to 63)
  // bit 12-14: promotion piece (from KNIGHT == 1 to QUEEN == 4)
  //
  // Castling moves follow "king captures rook" representation. So in case book
  // move is a promotion we have to convert to our representation, in all the
  // other cases we can directly compare with a Move after having masked out
  // the special Move's flags (bit 14-15) that are not supported by PolyGlot.
  //int pt = (move >> 12) & 7;
  //if (pt)
  //    move = make<PROMOTION>(from_sq(move), to_sq(move), PieceType(pt + 1));

  // Add 'special move' flags and verify it is legal
  for (MoveList<LEGAL> it(pos); *it; ++it)
      if (move == (*it ^ type_of(*it)))
          return *it;

  return MOVE_NONE;
}


/// find_first() takes a book key as input, and does a binary search through
/// the book file for the given key. Returns the index of the leftmost book
/// entry with the same key as the input.

size_t PolyglotBook::find_first(Key key) {

  seekg(0, ios::end); // Move pointer to end, so tellg() gets file's size

  size_t low = 0, mid, high = (size_t)tellg() / sizeof(Entry) - 1;
  Entry e;

  assert(low <= high);

  while (low < high && good())
  {
      mid = (low + high) / 2;

      assert(mid >= low && mid < high);

      seekg(mid * sizeof(Entry), ios_base::beg);
      *this >> e;

      if (key <= e.key)
          high = mid;
      else
          low = mid + 1;
  }

  assert(low == high);

  return low;
}
