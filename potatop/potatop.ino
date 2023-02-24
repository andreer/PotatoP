/* uLisp ARM Version 4.3a - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 18th September 2022
   
   Licensed under the MIT license: https://opensource.org/licenses/MIT
   
   With some small modifications by @andreer
   
   Error handling code by @Goheeca and @mgr from
   http://forum.ulisp.com/t/error-handling-in-ulisp/691/8
*/

/*

 TODO:
   - ensure vcom is _always_ toggled at min 1Hz, to keep the display from degrading!

*/

// Compile options

// #define resetautorun
#define printfreespace
// #define printgcs
#define sdcardsupport
#define gfxsupport
#define lisplibrary
#define assemblerlist
// #define lineeditor
// #define vt100

// Includes

#include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>

// Apollo3 specific stuff
#include "BurstMode.h"
#include "mbed.h"

mbed::ScopedRamExecutionLock make_ram_executable;

#if defined(gfxsupport)
#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_SharpMem.h> // Hardware-specific library for SHARP Memory displays
#include <Fonts/Picopixel.h>
#define COLOR_WHITE 1
#define COLOR_BLACK 0

#define SPI_FREQ 2000000

/* screen 1
#define SHARP_SCK   SPI_CLK
#define SHARP_MOSI  SPI_SDO
#define SHARP_MISO  SPI_SDI
#define SHARP_CS    D13
#define SHARP_VDD   D12
/* */

/* screen 2 */
#define SHARP_SCK   D42
#define SHARP_MOSI  D38
#define SHARP_MISO  D43
#define SHARP_CS    D36
#define SHARP_VDD   D44
/* */

MbedSPI mySPI(SHARP_MISO, SHARP_MOSI, SHARP_SCK); // declare the custom MbedSPI object mySPI
MbedSPI mySPI0(6, 7, 5); // declare the custom MbedSPI object mySPI
extern "C" SPIName spi_get_peripheral_name(PinName mosi, PinName miso, PinName sclk); // this mbed internal function determines the IOM module number for a set of pins

Adafruit_SharpMem tft(&mySPI, SHARP_CS, 320, 240, SPI_FREQ);
#endif


// Keyboard support

#define KEYEVENT_BUFFER_SIZE 100
volatile uint32_t keyEventWritePtr = 0;
volatile uint32_t keyEventReadPtr = 0;
volatile int16_t keyEvents[KEYEVENT_BUFFER_SIZE];
uint32_t keyEventNum = 0;

#define ROWS 8
#define COLS 14
#define DEBOUNCE_CYCLES 1 // doesn't really do much with how slow the scanning is ...
                          // scanning is now faster, I should look into this again

int rows[] = { 0, 1, 2, 45, 41, 17, 31, 16 };
int cols[] = { 18, 19, 15, 26, 9, 10, 8, 14, 35, 4, 22, 23, 27, 28 };

mbed::DigitalInOut* gpio[48];

#define __ "\xff"

const char Keymap[] PROGMEM =

// 0    1    2    3    4    5    6    7    8    9    a    b    c    d   //

   __  "."  "m"  "c"  "z"   __  "\n"  __   __  ","  "v"  "x"   __   __  // 0
   
   __  "l"  "j"  "d"  "a"   __  "\\"  __  ";"  "k"  "f"  "s"   __   __  // 1
   
   __   __  "n"   __   __   __  " "   __  "/"   __  "b"   __   __   __  // 2
   
   __   __  "h"   __  "\e"  __   __   __  "'"   __  "g"   __   __   __  // 3
   
   __   __  "y"   __  "\t"  __  "\b"  __  "["  "]"  "t"   __   __   __  // 4
   
"\x11" "9"  "7"  "3"  "1"   __   __"\x1f" "0"  "8"  "4"  "2"   __   __  // 5
   
   __  "o"  "u"  "e"  "q"   __   __"\x10" "p"  "i"  "r"  "w"   __   __  // 6
   
   __   __  "6"   __  "`"   __   __"\x1e" "-"  "="  "5"   __   __   __; // 7

const char KeymapShifted[] PROGMEM =

// 0    1    2    3    4    5    6    7    8    9    a    b    c    d   //

   __  ">"  "M"  "C"  "Z"   __  "\n"  __   __  "<"  "V"  "X"   __   __  // 0
   
   __  "L"  "J"  "D"  "A"   __  "|"   __  ":"  "K"  "F"  "S"   __   __  // 1
   
   __   __  "N"   __  "\e"  __  " "   __  "?"   __  "B"   __   __   __  // 2
   
   __   __  "H"   __   __   __   __   __  "\""  __  "G"   __   __   __  // 3
   
   __   __  "Y"   __  "\t"  __  "\b"  __  "{"  "}"  "T"   __   __   __  // 4
   
"\x11" "("  "&"  "#"  "!"   __   __"\x1f" ")"  "*"  "$"  "@"   __   __  // 5
   
   __  "O"  "U"  "E"  "Q"   __   __"\x10" "P"  "I"  "R"  "W"   __   __  // 6
   
   __   __  "^"   __  "~"   __   __"\x1e" "_"  "+"  "%"   __   __   __; // 7


uint8_t currentKeyState[COLS];
uint8_t reportedKeyState[COLS];
uint8_t reportKeyAgainIn[ROWS][COLS];


// Keyboard support end

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 91
#else
#define SDSIZE 0
#endif

// Platform specific settings

#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 36  // Number of bits+4
#define RAMFUNC __attribute__ ((section (".ramfunctions")))
#define MEMBANK

#if defined(ARDUINO_GEMMA_M0) || defined(ARDUINO_SEEED_XIAO_M0) || defined(ARDUINO_QTPY_M0)
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 4
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ADAFRUIT_FEATHER_M0)        /* Feather M0 without DataFlash */
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 4
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_METRO_M4) || defined(ARDUINO_ITSYBITSY_M4) || defined(ARDUINO_FEATHER_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
  #define WORKSPACESIZE (20608-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define SDCARD_SS_PIN 10
  #define STACKDIFF 400
  #define CPU_ATSAMD51

#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  #define WORKSPACESIZE (28800-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 8192000               /* 8 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 400
  #define CPU_ATSAMD51

#elif defined(ARDUINO_SAMD_MKRZERO)
  #define WORKSPACESIZE (2640-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 840
  #define CPU_ATSAMD21

#elif defined(ARDUINO_SAMD_ZERO)          /* Put this last, otherwise overrides the Adafruit boards */
  #define WORKSPACESIZE (2640-SDSIZE)     /* Objects (8*bytes) */
  #define EEPROMFLASH
  #define FLASHSIZE 32768                 /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 10
  #define STACKDIFF 320
  #define CPU_ATSAMD21

#elif defined(ARDUINO_BBC_MICROBIT)
  #define WORKSPACESIZE 1344              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_BBC_MICROBIT_V2)
  #define WORKSPACESIZE 12928              /* Objects (8*bytes) */
  #define CODESIZE 128                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF52833

#elif defined(ARDUINO_CALLIOPE_MINI)
  #define WORKSPACESIZE 3392              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_SINOBIT)
  #define WORKSPACESIZE 1344              /* Objects (8*bytes) */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320
  #define CPU_NRF51822

#elif defined(ARDUINO_NRF52840_ITSYBITSY) || defined(ARDUINO_NRF52840_CLUE)
  #define WORKSPACESIZE (21120-SDSIZE)    /* Objects (8*bytes) */
  #define DATAFLASH
  #define FLASHSIZE 2048000               /* 2 MBytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 1200
  #define CPU_NRF52840

#elif defined(MAX32620)
  #define WORKSPACESIZE (24704-SDSIZE)    /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_MAX32620
  #define Wire1 Wire2

#elif defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
  #define WORKSPACESIZE 60000             /* Objects (8*bytes) */
  #define LITTLEFS (960 * 1024)
  #include <LittleFS.h>
  LittleFS_Program LittleFS;
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 15000
  #define CPU_iMXRT1062
  #define SDCARD_SS_PIN BUILTIN_SDCARD
  #define BitOrder uint8_t
  #undef RAMFUNC
  #define RAMFUNC FASTRUN
  #undef MEMBANK
  #define MEMBANK DMAMEM

#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040)
  #define WORKSPACESIZE (22912-SDSIZE)    /* Objects (8*bytes) */
  #define LITTLEFS
  #include <LittleFS.h>
  #define FILE_WRITE_BEGIN "w"
  #define FILE_READ "r"
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_RP2040

#elif defined(ARDUINO_RASPBERRY_PI_PICO_W)
  #define WORKSPACESIZE (15872-SDSIZE)    /* Objects (8*bytes) */
  #define LITTLEFS
  #include <WiFi.h>
  #include <LittleFS.h>
  #define FILE_WRITE_BEGIN "w"
  #define FILE_READ "r"
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320
  #define CPU_RP2040

#elif defined(ARDUINO_APOLLO3_SFE_ARTEMIS_ATP)
  #define WORKSPACESIZE 30001
  #define SDCARD_SS_PIN D13
  #define CODESIZE 256
  #define STACKDIFF 320
  #define CPU_APOLLO3
#else
#error "Board not supported!"
#endif

// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           (((object *) (x))->car)
#define second(x)          (car(cdr(x)))
#define cddr(x)            (cdr(cdr(x)))
#define third(x)           (car(cdr(cdr(x))))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && (x)->type == FLOAT)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define arrayp(x)          ((x) != NULL && (x)->type == ARRAY)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

#define issp(x)            (x == ' ' || x == '\n' || x == '\r' || x == '\t')
#define longsymbolp(x)     (((x)->name & 0x03) == 0)
#define twist(x)           ((uint32_t)((x)<<2) | (((x) & 0xC0000000)>>30))
#define untwist(x)         (((x)>>2 & 0x3FFFFFFF) | ((x) & 0x03)<<30)
#define PACKEDS            0x43238000
#define BUILTINS           0xF4240000

// Code marker stores start and end of code block
#define startblock(x)      ((x->integer) & 0xFFFF)
#define endblock(x)        ((x->integer) >> 16 & 0xFFFF)

// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZZERO=0, SYMBOL=2, CODE=4, NUMBER=6, STREAM=8, CHARACTER=10, FLOAT=12, ARRAY=14, STRING=16, PAIR=18 };  // ARRAY STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM, STRINGSTREAM, GFXSTREAM };

// Stream names used by printobject
const char serialstream[] PROGMEM = "serial";
const char i2cstream[] PROGMEM = "i2c";
const char spistream[] PROGMEM = "spi";
const char sdstream[] PROGMEM = "sd";
const char wifistream[] PROGMEM = "wifi";
const char stringstream[] PROGMEM = "string";
const char gfxstream[] PROGMEM = "gfx";
const char *const streamname[] PROGMEM = {serialstream, i2cstream, spistream, sdstream, wifistream, stringstream, gfxstream};

// Typedefs

typedef uint32_t symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef const struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
  const char *doc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);

enum builtin_t { NIL, TEE, NOTHING, OPTIONAL, INITIALELEMENT, ELEMENTTYPE, BIT, AMPREST, LAMBDA, LET,
LETSTAR, CLOSURE, PSTAR, SPECIAL_FORMS, QUOTE, OR, DEFUN, DEFVAR, SETQ, LOOP, RETURN, PUSH, POP, INCF,
DECF, SETF, DOLIST, DOTIMES, TRACE, UNTRACE, FORMILLIS, TIME, WITHOUTPUTTOSTRING, WITHSERIAL, WITHI2C,
WITHSPI, WITHSDCARD, WITHGFX, WITHCLIENT, DEFCODE,
UNWINDPROTECT, IGNOREERRORS, SP_ERROR,
TAIL_FORMS, PROGN, IF, COND, WHEN, UNLESS, CASE, AND,
HELP, FUNCTIONS, NOT, NULLFN, CONS, ATOM, LISTP, CONSP, SYMBOLP, ARRAYP, BOUNDP, SETFN, STREAMP, EQ, CAR,
FIRST, CDR, REST, CAAR, CADR, SECOND, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, THIRD, CDAAR, CDADR, CDDAR,
CDDDR, LENGTH, ARRAYDIMENSIONS, LIST, MAKEARRAY, REVERSE, NTH, AREF, ASSOC, MEMBER, APPLY, FUNCALL,
APPEND, MAPC, MAPCAR, MAPCAN, ADD, SUBTRACT, MULTIPLY, DIVIDE, MOD, ONEPLUS, ONEMINUS, ABS, RANDOM, MAXFN,
MINFN, NOTEQ, NUMEQ, LESS, LESSEQ, GREATER, GREATEREQ, PLUSP, MINUSP, ZEROP, ODDP, EVENP, INTEGERP,
NUMBERP, FLOATFN, FLOATP, SIN, COS, TAN, ASIN, ACOS, ATAN, SINH, COSH, TANH, EXP, SQRT, LOG, EXPT,
CEILING, FLOOR, TRUNCATE, ROUND, CHAR, CHARCODE, CODECHAR, CHARACTERP, STRINGP, STRINGEQ, STRINGLESS,
STRINGGREATER, SORT, STRINGFN, CONCATENATE, SUBSEQ, READFROMSTRING, PRINCTOSTRING, PRIN1TOSTRING, LOGAND,
LOGIOR, LOGXOR, LOGNOT, ASH, LOGBITP, EVAL, GLOBALS, LOCALS, MAKUNBOUND, BREAK, READ, PRIN1, PRINT, PRINC,
TERPRI, READBYTE, READLINE, WRITEBYTE, WRITESTRING, WRITELINE, RESTARTI2C, GC, ROOM, SAVEIMAGE, LOADIMAGE,
CLS, PINMODE, DIGITALREAD, DIGITALWRITE, ANALOGREAD, ANALOGREFERENCE, ANALOGREADRESOLUTION, ANALOGWRITE,
ANALOGWRITERESOLUTION, DELAY, MILLIS, SLEEP, NOTE, REGISTER, EDIT, PPRINT, PPRINTALL, FORMAT, REQUIRE,
LISTLIBRARY, DOCUMENTATION, AVAILABLE, WIFISERVER, WIFISOFTAP, CONNECTED, WIFILOCALIP, WIFICONNECT,
DRAWPIXEL, DRAWLINE, DRAWRECT, FILLRECT, DRAWCIRCLE, FILLCIRCLE, DRAWROUNDRECT, FILLROUNDRECT,
DRAWTRIANGLE, FILLTRIANGLE, DRAWCHAR, SETCURSOR, SETTEXTCOLOR, SETTEXTSIZE, SETTEXTWRAP, FILLSCREEN,
SETROTATION, INVERTDISPLAY, KEYWORDS, 
K_LED_BUILTIN, K_HIGH, K_LOW,
#if defined(CPU_APOLLO3)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT,
#elif defined(CPU_ATSAMD21)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_AR_DEFAULT, K_AR_INTERNAL1V0, K_AR_INTERNAL1V65,
K_AR_INTERNAL2V23, K_AR_EXTERNAL, K_PA_DIR, K_PA_DIRCLR, K_PA_DIRSET, K_PA_DIRTGL, K_PA_OUT, K_PA_OUTCLR,
K_PA_OUTSET, K_PA_OUTTGL, K_PA_IN, K_PB_DIR, K_PB_DIRCLR, K_PB_DIRSET, K_PB_DIRTGL, K_PB_OUT, K_PB_OUTCLR,
K_PB_OUTSET, K_PB_OUTTGL, K_PB_IN,
#elif defined(CPU_ATSAMD51)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_AR_DEFAULT, K_AR_INTERNAL1V0, K_AR_INTERNAL1V1,
K_AR_INTERNAL1V2, K_AR_INTERNAL1V25, K_AR_INTERNAL1V65, K_AR_INTERNAL2V0, K_AR_INTERNAL2V2,
K_AR_INTERNAL2V23, K_AR_INTERNAL2V4, K_AR_INTERNAL2V5, K_AR_EXTERNAL, K_PA_DIR, K_PA_DIRCLR, K_PA_DIRSET,
K_PA_DIRTGL, K_PA_OUT, K_PA_OUTCLR, K_PA_OUTSET, K_PA_OUTTGL, K_PA_IN, K_PB_DIR, K_PB_DIRCLR, K_PB_DIRSET,
K_PB_DIRTGL, K_PB_OUT, K_PB_OUTCLR, K_PB_OUTSET, K_PB_OUTTGL, K_PB_IN,
#elif defined(CPU_NRF51822)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_AR_DEFAULT, K_AR_VBG, K_AR_SUPPLY_ONE_HALF,
K_AR_SUPPLY_ONE_THIRD, K_AR_EXT0, K_AR_EXT1, K_P0_OUT, K_P0_OUTSET, K_P0_OUTCLR, K_P0_IN, K_P0_DIR,
K_P0_DIRSET, K_P0_DIRCLR,
#elif defined(CPU_NRF52840)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_AR_DEFAULT, K_AR_INTERNAL, K_AR_INTERNAL_3_0,
K_AR_INTERNAL_2_4, K_AR_INTERNAL_1_8, K_AR_INTERNAL_1_2, K_AR_VDD4, K_P0_OUT, K_P0_OUTSET, K_P0_OUTCLR,
K_P0_IN, K_P0_DIR, K_P0_DIRSET, K_P0_DIRCLR, K_P1_OUT, K_P1_OUTSET, K_P1_OUTCLR, K_P1_IN, K_P1_DIR,
K_P1_DIRSET, K_P1_DIRCLR,
#elif defined(CPU_NRF52833)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_AR_DEFAULT, K_AR_INTERNAL, K_AR_VDD4, K_P0_OUT,
K_P0_OUTSET, K_P0_OUTCLR, K_P0_IN, K_P0_DIR, K_P0_DIRSET, K_P0_DIRCLR, K_P1_OUT, K_P1_OUTSET, K_P1_OUTCLR,
K_P1_IN, K_P1_DIR, K_P1_DIRSET, K_P1_DIRCLR,
#elif defined(CPU_iMXRT1062)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_OUTPUT_OPENDRAIN,
#elif defined(CPU_MAX32620)
K_INPUT, K_INPUT_PULLUP, K_OUTPUT, K_DEFAULT, K_EXTERNAL,
#elif defined(CPU_RP2040)
K_INPUT, K_INPUT_PULLUP, K_INPUT_PULLDOWN, K_OUTPUT, K_GPIO_IN, K_GPIO_OUT, K_GPIO_OUT_SET,
K_GPIO_OUT_CLR, K_GPIO_OUT_XOR, K_GPIO_OE, K_GPIO_OE_SET, K_GPIO_OE_CLR, K_GPIO_OE_XOR,
#endif
USERFUNCTIONS, GETERROR, REFRESH, GETKEY, PEEK, POKE, SUBSEQL, ENDFUNCTIONS, SET_SIZE = INT_MAX };

// Global variables

object Workspace[WORKSPACESIZE] WORDALIGNED MEMBANK;
#if defined(CODESIZE)
RAMFUNC uint8_t MyCode[CODESIZE] WORDALIGNED;
#endif

jmp_buf toplevel_handler;
jmp_buf *handler = &toplevel_handler;
unsigned int Freespace = 0;
object *Freelist;
unsigned int I2Ccount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
object *GlobalErrorString;
object *GlobalStringTail;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;

// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC, NOECHO, MUFFLEERRORS };
volatile uint8_t Flags = 0b00001; // PRINTREADABLY set by default

// Forward references
object *tee;

// Error handling

object *errorsub (symbol_t fname, PGM_P string) {
  if (!tstflag(MUFFLEERRORS)) {
    pfl(pserial); pfstring(PSTR("Error: "), pserial);
    if (fname != sym(NIL)) {
      pserial('\'');
      psymbol(fname, pserial);
      pserial('\''); pserial(' ');
    }
    pfstring(string, pserial);
  }

  // store error message in string object for GET-ERROR
  object *obj = startstring(SP_ERROR);
    if (fname != sym(NIL)) {
    pstr('\'');
    psymbol(fname, pstr);
    pstr('\''); pstr(' ');
  }
  pfstring(string, pstr);  // copy to globalstring

  return obj;
}

void errorsym (symbol_t fname, PGM_P string, object *symbol) {
  object *obj = errorsub(fname, string);
  if(!tstflag(MUFFLEERRORS)) {
    pserial(':'); pserial(' ');
    printobject(symbol, pserial);
    pln(pserial);
  }

  // add symbol to string object for GET-ERROR
  pstr(':'); pstr(' ');
  printobject(symbol, pstr);  // copy to globalstring
  // store error message in GlobalErrorString for GET-ERROR
  GlobalErrorString = obj;

  GCStack = NULL;
  longjmp(*handler, 1);
}

void errorsym2 (symbol_t fname, PGM_P string) {
  object *obj = errorsub(fname, string);

  if (!tstflag(MUFFLEERRORS)) {
    pln(pserial);
  }
  
  // store error message in GlobalErrorString for GET-ERROR
  GlobalErrorString = obj;

  GCStack = NULL;
  longjmp(*handler, 1);
}

void error (builtin_t fname, PGM_P string, object *symbol) {
  errorsym(sym(fname), string, symbol);
}

void error2 (builtin_t fname, PGM_P string) {
  errorsym2(sym(fname), string);
}

void formaterr (object *formatstr, PGM_P string, uint8_t p) {
  if (!tstflag(MUFFLEERRORS)) {
    pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
    indent(p+5, ' ', pserial); pserial('^');
    error2(FORMAT, string);
    pln(pserial);
  }
  GCStack = NULL;
  longjmp(*handler, 1);
}

// Save space as these are used multiple times
const char notanumber[] PROGMEM = "argument is not a number";
const char notaninteger[] PROGMEM = "argument is not an integer";
const char notastring[] PROGMEM = "argument is not a string";
const char notalist[] PROGMEM = "argument is not a list";
const char notasymbol[] PROGMEM = "argument is not a symbol";
const char notproper[] PROGMEM = "argument is not a proper list";
const char toomanyargs[] PROGMEM = "too many arguments";
const char toofewargs[] PROGMEM = "too few arguments";
const char noargument[] PROGMEM = "missing argument";
const char nostream[] PROGMEM = "missing stream argument";
const char overflow[] PROGMEM = "arithmetic overflow";
const char divisionbyzero[] PROGMEM = "division by zero";
const char indexnegative[] PROGMEM = "index can't be negative";
const char invalidarg[] PROGMEM = "invalid argument";
const char invalidkey[] PROGMEM = "invalid keyword";
const char illegalclause[] PROGMEM = "illegal clause";
const char invalidpin[] PROGMEM = "invalid pin";
const char oddargs[] PROGMEM = "odd number of arguments";
const char indexrange[] PROGMEM = "index out of range";
const char canttakecar[] PROGMEM = "can't take car";
const char canttakecdr[] PROGMEM = "can't take cdr";
const char unknownstreamtype[] PROGMEM = "unknown stream type";

// Set up workspace

void initworkspace () {
  Freelist = NULL;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
  }
}

object *myalloc () {
  if (Freespace == 0) error2(NIL, PSTR("no room"));
  object *temp = Freelist;
  Freelist = cdr(Freelist);
  Freespace--;
  return temp;
}

inline void myfree (object *obj) {
  car(obj) = NULL;
  cdr(obj) = Freelist;
  Freelist = obj;
  Freespace++;
}

// Make each type of object

object *number (int n) {
  object *ptr = myalloc();
  ptr->type = NUMBER;
  ptr->integer = n;
  return ptr;
}

object *makefloat (float f) {
  object *ptr = myalloc();
  ptr->type = FLOAT;
  ptr->single_float = f;
  return ptr;
}

object *character (uint8_t c) {
  object *ptr = myalloc();
  ptr->type = CHARACTER;
  ptr->chars = c;
  return ptr;
}

object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}

object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}

inline object *bsymbol (builtin_t name) {
  return intern(twist(name+BUILTINS));
}

object *codehead (int entry) {
  object *ptr = myalloc();
  ptr->type = CODE;
  ptr->integer = entry;
  return ptr;
}

object *intern (symbol_t name) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && obj->name == name) return obj;
  }
  return symbol(name);
}

bool eqsymbols (object *obj, char *buffer) {
  object *arg = cdr(obj);
  int i = 0;
  while (!(arg == NULL && buffer[i] == 0)) {
    if (arg == NULL || buffer[i] == 0 ||
      arg->chars != (buffer[i]<<24 | buffer[i+1]<<16 | buffer[i+2]<<8 | buffer[i+3])) return false;
    arg = car(arg);
    i = i + 4;
  }
  return true;
}

object *internlong (char *buffer) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && longsymbolp(obj) && eqsymbols(obj, buffer)) return obj;
  }
  object *obj = lispstring(buffer);
  obj->type = SYMBOL;
  return obj;
}

object *stream (uint8_t streamtype, uint8_t address) {
  object *ptr = myalloc();
  ptr->type = STREAM;
  ptr->integer = streamtype<<8 | address;
  return ptr;
}

object *newstring () {
  object *ptr = myalloc();
  ptr->type = STRING;
  ptr->chars = 0;
  return ptr;
}

// Garbage collection

void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);

  if (type >= PAIR || type == ZZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if (type == ARRAY) {
    obj = cdr(obj);
    goto MARK;
  }

  if ((type == STRING) || (type == SYMBOL && longsymbolp(obj))) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}

void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

void gc (object *form, object *env) {
  #if defined(printgcs)
  int start = Freespace;
  #endif
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  markobject(GlobalErrorString);
  sweep();
  #if defined(printgcs)
  pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
  #endif
}

// Compact image

void movepointer (object *from, object *to) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    unsigned int type = (obj->type) & ~MARKBIT;
    if (marked(obj) && (type >= ARRAY || type==ZZERO || (type == SYMBOL && longsymbolp(obj)))) {
      if (car(obj) == (object *)((uintptr_t)from | MARKBIT))
        car(obj) = (object *)((uintptr_t)to | MARKBIT);
      if (cdr(obj) == from) cdr(obj) = to;
    }
  }
  // Fix strings and long symbols
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (marked(obj)) {
      unsigned int type = (obj->type) & ~MARKBIT;
      if (type == STRING || (type == SYMBOL && longsymbolp(obj))) {
        obj = cdr(obj);
        while (obj != NULL) {
          if (cdr(obj) == to) cdr(obj) = from;
          obj = (object *)((uintptr_t)(car(obj)) & ~MARKBIT);
        }
      }
    }
  }
}

uintptr_t compactimage (object **arg) {
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  object *firstfree = Workspace;
  while (marked(firstfree)) firstfree++;
  object *obj = &Workspace[WORKSPACESIZE-1];
  while (firstfree < obj) {
    if (marked(obj)) {
      car(firstfree) = car(obj);
      cdr(firstfree) = cdr(obj);
      unmark(obj);
      movepointer(obj, firstfree);
      if (GlobalEnv == obj) GlobalEnv = firstfree;
      if (GCStack == obj) GCStack = firstfree;
      if (*arg == obj) *arg = firstfree;
      while (marked(firstfree)) firstfree++;
    }
    obj--;
  }
  sweep();
  return firstfree - Workspace;
}

// Make SD card filename

char *MakeFilename (object *arg, char *buffer) {
  int max = BUFFERSIZE-1;
  buffer[0]='/';
  int i = 1;
  do {
    char c = nthchar(arg, i-1);
    if (c == '\0') break;
    buffer[i++] = c;
  } while (i<max);
  buffer[i] = '\0';
  return buffer;
}

// Save-image and load-image

#if defined(sdcardsupport)
void SDWrite32 (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}

int SDRead32 (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#elif defined(LITTLEFS)
void FSWrite32 (File file, uint32_t data) {
  union { uint32_t data2; uint8_t u8[4]; };
  data2 = data;
  if (file.write(u8, 4) != 4) error2(SAVEIMAGE, PSTR("not enough room"));
}

uint32_t FSRead32 (File file) {
  union { uint32_t data; uint8_t u8[4]; };
  file.read(u8, 4);
  return data;
}
#elif defined(DATAFLASH)
// Winbond DataFlash support for Adafruit M4 Express boards
#define PAGEPROG      0x02
#define READSTATUS    0x05
#define READDATA      0x03
#define WRITEENABLE   0x06
#define BLOCK64K      0xD8
#define READID        0x90

// Arduino pins used for dataflash
#if defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
const int sck = 38, ssel = 39, mosi = 37, miso = 36;
#elif defined(EXTERNAL_FLASH_USE_QSPI)
const int sck = PIN_QSPI_SCK, ssel = PIN_QSPI_CS, mosi = PIN_QSPI_IO0, miso = PIN_QSPI_IO1;
#endif

void FlashBusy () {
  digitalWrite(ssel, 0);
  FlashWrite(READSTATUS);
  while ((FlashReadByte() & 1) != 0);
  digitalWrite(ssel, 1);
}

inline void FlashWrite (uint8_t data) {
  shiftOut(mosi, sck, MSBFIRST, data);
}

inline uint8_t FlashReadByte () {
  return shiftIn(miso, sck, MSBFIRST);
}

void FlashWriteByte (uint32_t *addr, uint8_t data) {
  // New page
  if (((*addr) & 0xFF) == 0) {
    digitalWrite(ssel, 1);
    FlashBusy();
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(PAGEPROG);
    FlashWrite((*addr)>>16);
    FlashWrite((*addr)>>8);
    FlashWrite(0);
  }
  FlashWrite(data);
  (*addr)++;
}

void FlashWriteEnable () {
  digitalWrite(ssel, 0);
  FlashWrite(WRITEENABLE);
  digitalWrite(ssel, 1);
}

bool FlashCheck () {
  uint8_t devID;
  digitalWrite(ssel, HIGH); pinMode(ssel, OUTPUT);
  pinMode(sck, OUTPUT);
  pinMode(mosi, OUTPUT);
  pinMode(miso, INPUT);
  digitalWrite(sck, LOW); digitalWrite(mosi, HIGH);
  digitalWrite(ssel, LOW);
  FlashWrite(READID);
  for (uint8_t i=0; i<4; i++) FlashReadByte();
  devID = FlashReadByte();
  digitalWrite(ssel, HIGH);
  return (devID == 0x14 || devID == 0x15 || devID == 0x16); // true = found correct device
}

void FlashBeginWrite (uint32_t *addr, uint32_t bytes) {
  *addr = 0;
  uint8_t blocks = (bytes+65535)/65536;
  // Erase 64K
  for (uint8_t b=0; b<blocks; b++) {
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(BLOCK64K);
    FlashWrite(b); FlashWrite(0); FlashWrite(0);
    digitalWrite(ssel, 1);
    FlashBusy();
  }
}

void FlashWrite32 (uint32_t *addr, uint32_t data) {
  FlashWriteByte(addr, data & 0xFF); FlashWriteByte(addr, data>>8 & 0xFF);
  FlashWriteByte(addr, data>>16 & 0xFF); FlashWriteByte(addr, data>>24 & 0xFF);
}

inline void FlashEndWrite (uint32_t *addr) {
  (void) addr;
  digitalWrite(ssel, 1);
  FlashBusy();
}

void FlashBeginRead (uint32_t *addr) {
  *addr = 0;
  FlashBusy();
  digitalWrite(ssel, 0);
  FlashWrite(READDATA);
  FlashWrite(0); FlashWrite(0); FlashWrite(0);
}

uint32_t FlashRead32 (uint32_t *addr) {
  (void) addr;
  uint8_t b0 = FlashReadByte(); uint8_t b1 = FlashReadByte();
  uint8_t b2 = FlashReadByte(); uint8_t b3 = FlashReadByte();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}

inline void FlashEndRead(uint32_t *addr) {
  (void) addr;
  digitalWrite(ssel, 1);
}

#elif defined(EEPROMFLASH)
// For ATSAMD21
__attribute__((__aligned__(256))) static const uint8_t flash_store[FLASHSIZE] = { };

void row_erase (const volatile void *addr) {
  NVMCTRL->ADDR.reg = ((uint32_t)addr) / 2;
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_ER;
  while (!NVMCTRL->INTFLAG.bit.READY);
}

void page_clear () {
  // Execute "PBC" Page Buffer Clear
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_PBC;
  while (NVMCTRL->INTFLAG.bit.READY == 0);
}

void page_write () {
  NVMCTRL->CTRLA.reg = NVMCTRL_CTRLA_CMDEX_KEY | NVMCTRL_CTRLA_CMD_WP;
  while (NVMCTRL->INTFLAG.bit.READY == 0);
}

bool FlashCheck() {
  return true;
}

void FlashBeginWrite(uint32_t *addr, uint32_t bytes) {
  (void) bytes;
  *addr = (uint32_t)flash_store;
  // Disable automatic page write
  NVMCTRL->CTRLB.bit.MANW = 1;
}

void FlashWrite32 (uint32_t *addr, uint32_t data) {
  if (((*addr) & 0xFF) == 0) row_erase((const volatile void *)(*addr));
  if (((*addr) & 0x3F) == 0) page_clear();
  *(volatile uint32_t *)(*addr) = data;
  (*addr) = (*addr) + 4;
  if (((*addr) & 0x3F) == 0) page_write();
}

void FlashEndWrite (uint32_t *addr) {
  if (((*addr) & 0x3F) != 0) page_write();
}

void FlashBeginRead(uint32_t *addr) {
  *addr = (uint32_t)flash_store;
}

uint32_t FlashRead32 (uint32_t *addr) {
  uint32_t data = *(volatile const uint32_t *)(*addr);
  (*addr) = (*addr) + 4;
  return data;
}

void FlashEndRead (uint32_t *addr) {
  (void) addr;
}
#endif

int saveimage (object *arg) {
#if defined(sdcardsupport)
  unsigned int imagesize = compactimage(&arg);
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer), O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card or invalid filename"));
    arg = NULL;
  } else if (arg == NULL || listp(arg)) {
    file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  }
  else error(SAVEIMAGE, invalidarg, arg);
  SDWrite32(file, (uintptr_t)arg);
  SDWrite32(file, imagesize);
  SDWrite32(file, (uintptr_t)GlobalEnv);
  SDWrite32(file, (uintptr_t)GCStack);
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWrite32(file, (uintptr_t)car(obj));
    SDWrite32(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(LITTLEFS)
  unsigned int imagesize = compactimage(&arg);
  LittleFS.begin(LITTLEFS);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = LittleFS.open(MakeFilename(arg, buffer), FILE_WRITE_BEGIN);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to LittleFS or invalid filename"));
    arg = NULL;
  } else if (arg == NULL || listp(arg)) {
    file = LittleFS.open("/ULISP.IMG", FILE_WRITE_BEGIN);
    if (!file) error2(SAVEIMAGE, PSTR("problem saving to LittleFS"));
  } else error(SAVEIMAGE, invalidarg, arg);
  FSWrite32(file, (uintptr_t)arg);
  FSWrite32(file, imagesize);
  FSWrite32(file, (uintptr_t)GlobalEnv);
  FSWrite32(file, (uintptr_t)GCStack);
  if (file.write(MyCode, CODESIZE) != CODESIZE) error2(SAVEIMAGE, PSTR("not enough room"));
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FSWrite32(file, (uintptr_t)car(obj));
    FSWrite32(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(DATAFLASH) || defined(EEPROMFLASH)
  unsigned int imagesize = compactimage(&arg);
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (!FlashCheck()) error2(SAVEIMAGE, PSTR("flash not available"));
  // Save to flash
  uint32_t bytesneeded = 16 + CODESIZE + imagesize*8;
  if (bytesneeded > FLASHSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr;
  FlashBeginWrite(&addr, bytesneeded);
  FlashWrite32(&addr, (uintptr_t)arg);
  FlashWrite32(&addr, imagesize);
  FlashWrite32(&addr, (uintptr_t)GlobalEnv);
  FlashWrite32(&addr, (uintptr_t)GCStack);
  for (int i=0; i<CODESIZE; i=i+4) {
    union { uint32_t u32; uint8_t u8[4]; };
    u8[0] = MyCode[i]; u8[1] = MyCode[i+1]; u8[2] = MyCode[i+2]; u8[3] = MyCode[i+3];
    FlashWrite32(&addr, u32);
  }
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWrite32(&addr, (uintptr_t)car(obj));
    FlashWrite32(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite(&addr);
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = SD.open(MakeFilename(arg, buffer));
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card or invalid filename"));
  }
  else if (arg == NULL) {
    file = SD.open("/ULISP.IMG");
    if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  }
  else error(LOADIMAGE, invalidarg, arg);
  SDRead32(file);
  unsigned int imagesize = SDRead32(file);
  GlobalEnv = (object *)SDRead32(file);
  GCStack = (object *)SDRead32(file);
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDRead32(file);
    cdr(obj) = (object *)SDRead32(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(LITTLEFS)
  LittleFS.begin(LITTLEFS);
  File file;
  if (stringp(arg)) {
    char buffer[BUFFERSIZE];
    file = LittleFS.open(MakeFilename(arg, buffer), FILE_READ);
    if (!file) error2(LOADIMAGE, PSTR("problem loading from LittleFS or invalid filename"));
  }
  else if (arg == NULL) {
    file = LittleFS.open("/ULISP.IMG", FILE_READ);
    if (!file) error2(LOADIMAGE, PSTR("problem loading from LittleFS"));
  }
  else error(LOADIMAGE, invalidarg, arg);
  FSRead32(file);
  unsigned int imagesize = FSRead32(file);
  GlobalEnv = (object *)FSRead32(file);
  GCStack = (object *)FSRead32(file);
  file.read(MyCode, CODESIZE);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FSRead32(file);
    cdr(obj) = (object *)FSRead32(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(DATAFLASH) || defined(EEPROMFLASH) || defined(EEPROMLIBRARY)
  (void) arg;
  if (!FlashCheck()) error2(LOADIMAGE, PSTR("flash not available"));
  uint32_t addr;
  FlashBeginRead(&addr);
  FlashRead32(&addr); // Skip eval address
  uint32_t imagesize = FlashRead32(&addr);
  if (imagesize == 0 || imagesize == 0xFFFFFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashRead32(&addr);
  GCStack = (object *)FlashRead32(&addr);
  for (int i=0; i<CODESIZE; i=i+4) {
    union { uint32_t u32; uint8_t u8[4]; };
    u32 = FlashRead32(&addr);
    MyCode[i] = u8[0]; MyCode[i+1] = u8[1]; MyCode[i+2] = u8[2]; MyCode[i+3] = u8[3];
  }
  for (uint32_t i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashRead32(&addr);
    cdr(obj) = (object *)FlashRead32(&addr);
  }
  FlashEndRead(&addr);
  gc(NULL, NULL);
  return imagesize;
#else
  (void) arg;
  error2(LOADIMAGE, PSTR("not available"));
  return 0;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("/ULISP.IMG");
  if (!file) error2(NIL, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDRead32(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(NIL, autorun, NULL, NULL);
  }
#elif defined(LITTLEFS)
  LittleFS.begin(LITTLEFS);
  File file = LittleFS.open("/ULISP.IMG", FILE_READ);
  if (!file) error2(NIL, PSTR("problem autorunning from LittleFS"));
  object *autorun = (object *)FSRead32(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(NIL, autorun, NULL, NULL);
  }
#elif defined(DATAFLASH) || defined(EEPROMFLASH) || defined(EEPROMLIBRARY)
  if (!FlashCheck()) error2(NIL, PSTR("flash not available"));
  uint32_t addr;
  FlashBeginRead(&addr);
  object *autorun = (object *)FlashRead32(&addr);
  FlashEndRead(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFFFFFF) {
    loadimage(nil);
    apply(NIL, autorun, NULL, NULL);
  }
#else
  error2(NIL, PSTR("autorun not available"));
#endif
}

// Tracing

int tracing (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) return i+1;
    i++;
  }
  return 0;
}

void trace (symbol_t name) {
  if (tracing(name)) error(TRACE, PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(TRACE, PSTR("already tracing 3 functions"));
}

void untrace (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) { TraceFn[i] = 0; return; }
    i++;
  }
  error(UNTRACE, PSTR("not tracing"), symbol(name));
}

// Helper functions

bool consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

#define atom(x) (!consp(x))

bool listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

#define improperp(x) (!listp(x))

object *quote (object *arg) {
  return cons(bsymbol(QUOTE), cons(arg,NULL));
}

// Radix 40 encoding

builtin_t builtin (symbol_t name) {
  return (builtin_t)(untwist(name) - BUILTINS);
}

symbol_t sym (builtin_t x) {
  return twist(x + BUILTINS);
}

int8_t toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+1;
  if (ch == '-') return 37; if (ch == '*') return 38; if (ch == '$') return 39;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+11;
  return -1; // Invalid
}

char fromradix40 (char n) {
  if (n >= 1 && n <= 9) return '0'+n-1;
  if (n >= 11 && n <= 36) return 'a'+n-11;
  if (n == 37) return '-'; if (n == 38) return '*'; if (n == 39) return '$';
  return 0;
}

uint32_t pack40 (char *buffer) {
  int x = 0;
  for (int i=0; i<6; i++) x = x * 40 + toradix40(buffer[i]);
  return x;
}

bool valid40 (char *buffer) {
  if (toradix40(buffer[0]) < 11) return false;
  for (int i=1; i<6; i++) if (toradix40(buffer[i]) < 0) return false;
  return true;
}

int8_t digitvalue (char d) {
  if (d>='0' && d<='9') return d-'0';
  d = d | 0x20;
  if (d>='a' && d<='f') return d-'a'+10;
  return 16;
}

int checkinteger (builtin_t name, object *obj) {
  if (!integerp(obj)) error(name, notaninteger, obj);
  return obj->integer;
}

int checkbitvalue (builtin_t name, object *obj) {
  if (!integerp(obj)) error(name, notaninteger, obj);
  int n = obj->integer;
  if (n & ~1) error(name, PSTR("argument is not a bit value"), obj);
  return n;
}

float checkintfloat (builtin_t name, object *obj){
  if (integerp(obj)) return obj->integer;
  if (!floatp(obj)) error(name, notanumber, obj);
  return obj->single_float;
}

int checkchar (builtin_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return obj->chars;
}

object *checkstring (builtin_t name, object *obj) {
  if (!stringp(obj)) error(name, notastring, obj);
  return obj;
}

int isstream (object *obj){
  if (!streamp(obj)) error(NIL, PSTR("not a stream"), obj);
  return obj->integer;
}

int isbuiltin (object *obj, builtin_t n) {
  return symbolp(obj) && obj->name == sym(n);
}

bool builtinp (symbol_t name) {
  return (untwist(name) > BUILTINS && untwist(name) < ENDFUNCTIONS+BUILTINS);
}

int keywordp (object *obj) {
  if (!symbolp(obj)) return false;
  builtin_t name = builtin(obj->name);
  return ((name > KEYWORDS) && (name < USERFUNCTIONS));
}

int checkkeyword (builtin_t name, object *obj) {
  if (!keywordp(obj)) error(name, PSTR("argument is not a keyword"), obj);
  builtin_t kname = builtin(obj->name);
  uint8_t context = getminmax(kname);
  if (context != 0 && context != name) error(name, invalidkey, obj);
  return ((int)lookupfn(kname));
}

void checkargs (builtin_t name, object *args) {
  int nargs = listlength(name, args);
  checkminmax(name, nargs);
}

int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}

int listlength (builtin_t name, object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(name, notproper);
    list = cdr(list);
    length++;
  }
  return length;
}

// Mathematical helper functions

object *add_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult + checkintfloat(ADD, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *subtract_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult - checkintfloat(SUBTRACT, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *negate (object *arg) {
  if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(-result);
    else return number(-result);
  } else if (floatp(arg)) return makefloat(-(arg->single_float));
  else error(SUBTRACT, notanumber, arg);
  return nil;
}

object *multiply_floats (object *args, float fresult) {
  while (args != NULL) {
   object *arg = car(args);
    fresult = fresult * checkintfloat(MULTIPLY, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *divide_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    float f = checkintfloat(DIVIDE, arg);
    if (f == 0.0) error2(DIVIDE, divisionbyzero);
    fresult = fresult / f;
    args = cdr(args);
  }
  return makefloat(fresult);
}

int myround (float number) {
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}

object *compare (builtin_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!lt && ((arg1->integer) < (arg2->integer))) return nil;
      if (!eq && ((arg1->integer) == (arg2->integer))) return nil;
      if (!gt && ((arg1->integer) > (arg2->integer))) return nil;
    } else {
      if (!lt && (checkintfloat(name, arg1) < checkintfloat(name, arg2))) return nil;
      if (!eq && (checkintfloat(name, arg1) == checkintfloat(name, arg2))) return nil;
      if (!gt && (checkintfloat(name, arg1) > checkintfloat(name, arg2))) return nil;
    }
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

int intpower (int base, int exp) {
  int result = 1;
  while (exp) {
    if (exp & 1) result = result * base;
    exp = exp / 2;
    base = base * base;
  }
  return result;
}

// Association lists

object *assoc (object *key, object *list) {
  while (list != NULL) {
    if (improperp(list)) error(ASSOC, notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(ASSOC, PSTR("element is not a list"), pair);
    if (pair != NULL && eq(key,car(pair))) return pair;
    list = cdr(list);
  }
  return nil;
}

object *delassoc (object *key, object **alist) {
  object *list = *alist;
  object *prev = NULL;
  while (list != NULL) {
    object *pair = first(list);
    if (eq(key,car(pair))) {
      if (prev == NULL) *alist = cdr(list);
      else cdr(prev) = cdr(list);
      return key;
    }
    prev = list;
    list = cdr(list);
  }
  return nil;
}

// Array utilities

int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n<2 ? 2 : n;
}

object *buildarray (int n, int s, object *def) {
  int s2 = s>>1;
  if (s2 == 1) {
    if (n == 2) return cons(def, def);
    else if (n == 1) return cons(def, NULL);
    else return NULL;
  } else if (n >= s2) return cons(buildarray(s2, s2, def), buildarray(n - s2, s2, def));
  else return cons(buildarray(n, s2, def), nil);
}

object *makearray (builtin_t name, object *dims, object *def, bool bitp) {
  int size = 1;
  object *dimensions = dims;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) error2(name, PSTR("dimension can't be negative"));
    size = size * d;
    dims = cdr(dims);
  }
  // Bit array identified by making first dimension negative
  if (bitp) {
    size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
    car(dimensions) = number(-(car(dimensions)->integer));
  }
  object *ptr = myalloc();
  ptr->type = ARRAY;
  object *tree = nil;
  if (size != 0) tree = buildarray(size, nextpower2(size), def);
  ptr->cdr = cons(tree, dimensions);
  return ptr;
}

object **arrayref (object *array, int index, int size) {
  int mask = nextpower2(size)>>1;
  object **p = &car(cdr(array));
  while (mask) {
    if ((index & mask) == 0) p = &(car(*p)); else p = &(cdr(*p));
    mask = mask>>1;
  }
  return p;
}

object **getarray (builtin_t name, object *array, object *subs, object *env, int *bit) {
  int index = 0, size = 1, s;
  *bit = -1;
  bool bitp = false;
  object *dims = cddr(array);
  while (dims != NULL && subs != NULL) {
    int d = car(dims)->integer;
    if (d < 0) { d = -d; bitp = true; }
    if (env) s = checkinteger(name, eval(car(subs), env)); else s = checkinteger(name, car(subs));
    if (s < 0 || s >= d) error(name, PSTR("subscript out of range"), car(subs));
    size = size * d;
    index = index * d + s;
    dims = cdr(dims); subs = cdr(subs);
  }
  if (dims != NULL) error2(name, PSTR("too few subscripts"));
  if (subs != NULL) error2(name, PSTR("too many subscripts"));
  if (bitp) {
    size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
    *bit = index & (sizeof(int)==4 ? 0x1F : 0x0F);
    index = index>>(sizeof(int)==4 ? 5 : 4);
  }
  return arrayref(array, index, size);
}

void rslice (object *array, int size, int slice, object *dims, object *args) {
  int d = first(dims)->integer;
  for (int i = 0; i < d; i++) {
    int index = slice * d + i;
    if (!consp(args)) error2(NIL, PSTR("initial contents don't match array type"));
    if (cdr(dims) == NULL) {
      object **p = arrayref(array, index, size);
      *p = car(args);
    } else rslice(array, size, index, cdr(dims), car(args));
    args = cdr(args);
  }
}

object *readarray (int d, object *args) {
  object *list = args;
  object *dims = NULL; object *head = NULL;
  int size = 1;
  for (int i = 0; i < d; i++) {
    if (!listp(list)) error2(NIL, PSTR("initial contents don't match array type"));
    int l = listlength(NIL, list);
    if (dims == NULL) { dims = cons(number(l), NULL); head = dims; }
    else { cdr(dims) = cons(number(l), NULL); dims = cdr(dims); }
    size = size * l;
    if (list != NULL) list = car(list);
  }
  object *array = makearray(NIL, head, NULL, false);
  rslice(array, size, 0, head, args);
  return array;
}

object *readbitarray (gfun_t gfun) {
  char ch = gfun();
  object *head = NULL;
  object *tail = NULL;
  while (!issp(ch) && ch != ')' && ch != '(') {
    if (ch != '0' && ch != '1') error2(NIL, PSTR("illegal character in bit array"));
    object *cell = cons(number(ch - '0'), NULL);
    if (head == NULL) head = cell;
    else tail->cdr = cell;
    tail = cell;
    ch = gfun();
  }
  LastChar = ch;
  int size = listlength(NIL, head);
  object *array = makearray(NIL, cons(number(size), NULL), 0, true);
  size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
  int index = 0;
  while (head != NULL) {
    object **loc = arrayref(array, index>>(sizeof(int)==4 ? 5 : 4), size);
    int bit = index & (sizeof(int)==4 ? 0x1F : 0x0F);
    *loc = number((((*loc)->integer) & ~(1<<bit)) | (car(head)->integer)<<bit);
    index++;
    head = cdr(head);
  }
  return array;
}

void pslice (object *array, int size, int slice, object *dims, pfun_t pfun, bool bitp) {
  bool spaces = true;
  if (slice == -1) { spaces = false; slice = 0; }
  int d = first(dims)->integer;
  if (d < 0) d = -d;
  for (int i = 0; i < d; i++) {
    if (i && spaces) pfun(' ');
    int index = slice * d + i;
    if (cdr(dims) == NULL) {
      if (bitp) pint(((*arrayref(array, index>>(sizeof(int)==4 ? 5 : 4), size))->integer)>>
        (index & (sizeof(int)==4 ? 0x1F : 0x0F)) & 1, pfun);
      else printobject(*arrayref(array, index, size), pfun);
    } else { pfun('('); pslice(array, size, index, cdr(dims), pfun, bitp); pfun(')'); }
  }
}

void printarray (object *array, pfun_t pfun) {
  object *dimensions = cddr(array);
  object *dims = dimensions;
  bool bitp = false;
  int size = 1, n = 0;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) { bitp = true; d = -d; }
    size = size * d;
    dims = cdr(dims); n++;
  }
  if (bitp) size = (size + sizeof(int)*8 - 1)/(sizeof(int)*8);
  pfun('#');
  if (n == 1 && bitp) { pfun('*'); pslice(array, size, -1, dimensions, pfun, bitp); }
  else {
    if (n > 1) { pint(n, pfun); pfun('A'); }
    pfun('('); pslice(array, size, 0, dimensions, pfun, bitp); pfun(')');
  }
}

// String utilities

void indent (uint8_t spaces, char ch, pfun_t pfun) {
  for (uint8_t i=0; i<spaces; i++) pfun(ch);
}

object *startstring (builtin_t name) {
  (void) name;
  object *string = newstring();
  GlobalString = string;
  GlobalStringTail = string;
  return string;
}

void buildstring (char ch, object **tail) {
  object *cell;
  if (cdr(*tail) == NULL) {
    cell = myalloc(); cdr(*tail) = cell;
  } else if (((*tail)->chars & 0xFFFFFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch<<16; return;
  } else if (((*tail)->chars & 0xFFFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch<<8; return;
  } else if (((*tail)->chars & 0xFF) == 0) {
    (*tail)->chars = (*tail)->chars | ch; return;
  } else {
    cell = myalloc(); car(*tail) = cell;
  }
  car(cell) = NULL; cell->chars = ch<<24; *tail = cell;
}

object *copystring (object *arg) {
  object *obj = newstring();
  object *ptr = obj;
  arg = cdr(arg);
  while (arg != NULL) {
    object *cell =  myalloc(); car(cell) = NULL;
    if (cdr(obj) == NULL) cdr(obj) = cell; else car(ptr) = cell;
    ptr = cell;
    ptr->chars = arg->chars;
    arg = car(arg);
  }
  return obj;
}

object *readstring (uint8_t delim, gfun_t gfun, bool ignoreBackslash) {
  object *obj = newstring();
  object *tail = obj;
  int ch = gfun();
  if (ch == -1) return nil;
  while ((ch != delim) && (ch != -1)) {
    if (ignoreBackslash && ch == '\\') ch = gfun();
    buildstring(ch, &tail);
    ch = gfun();
  }
  return obj;
}

int stringlength (object *form) {
  int length = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      if (chars>>i & 0xFF) length++;
    }
    form = car(form);
  }
  return length;
}

uint8_t nthchar (object *string, int n) {
  object *arg = cdr(string);
  int top;
  if (sizeof(int) == 4) { top = n>>2; n = 3 - (n&3); }
  else { top = n>>1; n = 1 - (n&1); }
  for (int i=0; i<top; i++) {
    if (arg == NULL) return 0;
    arg = car(arg);
  }
  if (arg == NULL) return 0;
  return (arg->chars)>>(n*8) & 0xFF;
}

int gstr () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = nthchar(GlobalString, GlobalStringIndex++);
  if (c != 0) return c;
  return '\n'; // -1?
}

void pstr (char c) {
  buildstring(c, &GlobalStringTail);
}

object *lispstring (char *s) {
  object *obj = newstring();
  object *tail = obj;
  while(1) {
    char ch = *s++;
    if (ch == 0) break;
    if (ch == '\\') ch = *s++;
    buildstring(ch, &tail);
  }
  return obj;
}

bool stringcompare (builtin_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = checkstring(name, first(args));
  object *arg2 = checkstring(name, second(args));
  arg1 = cdr(arg1);
  arg2 = cdr(arg2);
  while ((arg1 != NULL) || (arg2 != NULL)) {
    if (arg1 == NULL) return lt;
    if (arg2 == NULL) return gt;
    if (arg1->chars < arg2->chars) return lt;
    if (arg1->chars > arg2->chars) return gt;
    arg1 = car(arg1);
    arg2 = car(arg2);
  }
  return eq;
}

object *documentation (builtin_t name, object *arg, object *env) {
  if (!symbolp(arg)) error(name, notasymbol, arg);
  object *pair = findpair(arg, env);
  if (pair != NULL) {
    object *val = cdr(pair);
    if (listp(val) && first(val)->name == sym(LAMBDA) && cdr(val) != NULL && cddr(val) != NULL) {
      if (stringp(third(val))) return third(val);
    }
  }
  symbol_t docname = arg->name;
  if (!builtinp(docname)) return nil;
  char *docstring = lookupdoc(builtin(docname));
  if (docstring == NULL) return nil;
  return lispstring(docstring);
}

char *cstring (builtin_t name, object *form, char *buffer, int buflen) {
  form = cdr(checkstring(name, form));
  int index = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (index >= buflen-1) error2(NIL, PSTR("no room for string"));
        buffer[index++] = ch;
      }
    }
    form = car(form);
  }
  buffer[index] = '\0';
  return buffer;
}

uint32_t ipstring (builtin_t name, object *form) {
  form = cdr(checkstring(name, form));
  int p = 0;
  union { uint32_t ipaddress; uint8_t ipbytes[4]; } ;
  ipaddress = 0;
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (ch == '.') { p++; if (p > 3) error2(name, PSTR("illegal IP address")); }
        else ipbytes[p] = (ipbytes[p] * 10) + ch - '0';
      }
    }
    form = car(form);
  }
  return ipaddress;
}

// Lookup variable in environment

object *value (symbol_t n, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair)->name == n) return pair;
    env = cdr(env);
  }
  return nil;
}

object *findpair (object *var, object *env) {
  symbol_t name = var->name;
  object *pair = value(name, env);
  if (pair == NULL) pair = value(name, GlobalEnv);
  return pair;
}

bool boundp (object *var, object *env) {
  return (findpair(var, env) != NULL);
}

object *findvalue (builtin_t name, object *var, object *env) {
  object *pair = findpair(var, env);
  if (pair == NULL) error(name, PSTR("unknown variable"), var);
  return pair;
}

// Handling closures

object *closure (int tc, symbol_t name, object *function, object *args, object **env) {
  object *state = car(function);
  function = cdr(function);
  int trace = 0;
  if (name) trace = tracing(name);
  if (trace) {
    indent(TraceDepth[trace-1]<<1, ' ', pserial);
    pint(TraceDepth[trace-1]++, pserial);
    pserial(':'); pserial(' '); pserial('('); printsymbol(symbol(name), pserial);
  }
  object *params = first(function);
  if (!listp(params)) errorsym(name, notalist, params);
  function = cdr(function);
  // Dropframe
  if (tc) {
    if (*env != NULL && car(*env) == NULL) {
      pop(*env);
      while (*env != NULL && car(*env) != NULL) pop(*env);
    } else push(nil, *env);
  }
  // Push state
  while (consp(state)) {
    object *pair = first(state);
    push(pair, *env);
    state = cdr(state);
  }
  // Add arguments to environment
  bool optional = false;
  while (params != NULL) {
    object *value;
    object *var = first(params);
    if (isbuiltin(var, OPTIONAL)) optional = true;
    else {
      if (consp(var)) {
        if (!optional) errorsym(name, PSTR("invalid default value"), var);
        if (args == NULL) value = eval(second(var), *env);
        else { value = first(args); args = cdr(args); }
        var = first(var);
        if (!symbolp(var)) errorsym(name, PSTR("illegal optional parameter"), var);
      } else if (!symbolp(var)) {
        errorsym(name, PSTR("illegal function parameter"), var);
      } else if (isbuiltin(var, AMPREST)) {
        params = cdr(params);
        var = first(params);
        value = args;
        args = NULL;
      } else {
        if (args == NULL) {
          if (optional) value = nil;
          else errorsym2(name, toofewargs);
        } else { value = first(args); args = cdr(args); }
      }
      push(cons(var,value), *env);
      if (trace) { pserial(' '); printobject(value, pserial); }
    }
    params = cdr(params);
  }
  if (args != NULL) errorsym2(name, toomanyargs);
  if (trace) { pserial(')'); pln(pserial); }
  // Do an implicit progn
  if (tc) push(nil, *env);
  return tf_progn(function, *env);
}

object *apply (builtin_t name, object *function, object *args, object *env) {
  if (symbolp(function)) {
    builtin_t fname = builtin(function->name);
    if ((fname > FUNCTIONS) && (fname < KEYWORDS)) {
      checkargs(fname, args);
      return ((fn_ptr_type)lookupfn(fname))(args, env);
    } else function = eval(function, env);
  }
  if (consp(function) && isbuiltin(car(function), LAMBDA)) {
    object *result = closure(0, sym(name), function, args, &env);
    return eval(result, env);
  }
  if (consp(function) && isbuiltin(car(function), CLOSURE)) {
    function = cdr(function);
    object *result = closure(0, sym(name), function, args, &env);
    return eval(result, env);
  }
  error(name, PSTR("illegal function"), function);
  return NULL;
}

// In-place operations

object **place (builtin_t name, object *args, object *env, int *bit) {
  *bit = -1;
  if (atom(args)) return &cdr(findvalue(name, args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t sname = function->name;
    if (sname == sym(CAR) || sname == sym(FIRST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecar, value);
      return &car(value);
    }
    if (sname == sym(CDR) || sname == sym(REST)) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, canttakecdr, value);
      return &cdr(value);
    }
    if (sname == sym(NTH)) {
      int index = checkinteger(NTH, eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
      while (index > 0) {
        list = cdr(list);
        if (list == NULL) error2(name, PSTR("index to nth is out of range"));
        index--;
      }
      return &car(list);
    }
    if (sname == sym(AREF)) {
      object *array = eval(second(args), env);
      if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
      return getarray(AREF, array, cddr(args), env, bit);
    }
  }
  error2(name, PSTR("illegal place"));
  return nil;
}

// Checked car and cdr

object *carx (object *arg) {
  if (!listp(arg)) error(NIL, canttakecar, arg);
  if (arg == nil) return nil;
  return car(arg);
}

object *cdrx (object *arg) {
  if (!listp(arg)) error(NIL, canttakecdr, arg);
  if (arg == nil) return nil;
  return cdr(arg);
}

object *cxxxr (object *args, uint8_t pattern) {
  object *arg = first(args);
  while (pattern != 1) {
    if ((pattern & 1) == 0) arg = carx(arg); else arg = cdrx(arg);
    pattern = pattern>>1;
  }
  return arg;
}

// Mapping helper functions

void mapcarfun (object *result, object **tail) {
  object *obj = cons(result,NULL);
  cdr(*tail) = obj; *tail = obj;
}

void mapcanfun (object *result, object **tail) {
  if (cdr(*tail) != NULL) error(MAPCAN, notproper, *tail);
  while (consp(result)) {
    cdr(*tail) = result; *tail = result;
    result = cdr(result);
  }
}

object *mapcarcan (builtin_t name, object *args, object *env, mapfun_t fun) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL);
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(name, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(name, function, cdr(params), env);
    fun(result, &tail);
  }
}

// I2C interface for up to two ports, using Arduino Wire

void I2Cinit (TwoWire *port, bool enablePullup) {
  (void) enablePullup;
  port->begin();
}

int I2Cread (TwoWire *port) {
  return port->read();
}

void I2Cwrite (TwoWire *port, uint8_t data) {
  port->write(data);
}

bool I2Cstart (TwoWire *port, uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   port->beginTransmission(address);
   ok = (port->endTransmission(true) == 0);
   port->beginTransmission(address);
 }
 else port->requestFrom(address, I2Ccount);
 return ok;
}

bool I2Crestart (TwoWire *port, uint8_t address, uint8_t read) {
  int error = (port->endTransmission(false) != 0);
  if (read == 0) port->beginTransmission(address);
  else port->requestFrom(address, I2Ccount);
  return error ? false : true;
}

void I2Cstop (TwoWire *port, uint8_t read) {
  if (read == 0) port->endTransmission(); // Check for error?
}

// Streams

// Simplify board differences
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_SPI1
#endif
#if defined(ARDUINO_BBC_MICROBIT_V2) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040)
#define ULISP_I2C1
#endif
#if defined(ARDUINO_SAM_DUE) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
#define ULISP_SERIAL3
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_SERIAL2
#elif !defined(CPU_NRF51822) && !defined(CPU_NRF52833) && !defined(ARDUINO_FEATHER_F405)
#define ULISP_SERIAL1
#endif
#if defined(ARDUINO_RASPBERRY_PI_PICO_W)
#define ULISP_WIFI
#endif

inline int spiread () { return SPI.transfer(0); }
#if defined(ULISP_SPI1)
inline int spi1read () { return SPI1.transfer(0); }
#endif
inline int i2cread () { return I2Cread(&Wire); }
#if defined(ULISP_I2C1)
inline int i2c1read () { return I2Cread(&Wire1); }
#endif
#if defined(ULISP_SERIAL3)
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#endif
#if defined(ULISP_SERIAL3) || defined(ULISP_SERIAL2)
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
#endif
#if defined(ULISP_SERIAL3) || defined(ULISP_SERIAL2) || defined(ULISP_SERIAL1)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#endif
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return SDgfile.read();
}
#endif

#if defined(ULISP_WIFI)
WiFiClient client;
WiFiServer server(80);

inline int WiFiread () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return client.read();
}
#endif

void serialbegin (int address, int baud) {
  #if defined(ULISP_SERIAL3)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  #elif defined(ULISP_SERIAL2)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  #elif defined(ULISP_SERIAL1)
  if (address == 1) Serial1.begin((long)baud*100);
  #else
  (void) baud;
  if (false);
  #endif
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
}

void serialend (int address) {
  #if defined(ULISP_SERIAL3)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #elif defined(ULISP_SERIAL2)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  #elif defined(ULISP_SERIAL1)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #else
  if (false);
  #endif
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) {
    if (address < 128) gfun = i2cread;
    #if defined(ULISP_I2C1)
    else gfun = i2c1read;
    #endif
  } else if (streamtype == SPISTREAM) {
    if (address < 128) gfun = spiread;
    #if defined(ULISP_SPI1)
    else gfun = spi1read;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(ULISP_SERIAL3)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #elif defined(ULISP_SERIAL2)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    #elif defined(ULISP_SERIAL1)
    else if (address == 1) gfun = serial1read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  #if defined(ULISP_WIFI)
  else if (streamtype == WIFISTREAM) gfun = (gfun_t)WiFiread;
  #endif
  else error2(NIL, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(ULISP_SPI1)
inline void spi1write (char c) { SPI1.transfer(c); }
#endif
inline void i2cwrite (char c) { I2Cwrite(&Wire, c); }
#if defined(ULISP_I2C1)
inline void i2c1write (char c) { I2Cwrite(&Wire1, c); }
#endif
#if defined(ULISP_SERIAL3)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#elif defined(ULISP_SERIAL2)
inline void serial2write (char c) { Serial2.write(c); }
inline void serial1write (char c) { Serial1.write(c); }
#elif defined(ULISP_SERIAL1)
inline void serial1write (char c) { Serial1.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(ULISP_WIFI)
inline void WiFiwrite (char c) { client.write(c); }
#endif
#if defined(gfxsupport)
inline void gfxwrite (char c) { tft.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) {
    if (address < 128) pfun = i2cwrite;
    #if defined(ULISP_I2C1)
    else pfun = i2c1write;
    #endif
  } else if (streamtype == SPISTREAM) {
    if (address < 128) pfun = spiwrite;
    #if defined(ULISP_SPI1)
    else pfun = spi1write;
    #endif
  } else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(ULISP_SERIAL3)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #elif defined(ULISP_SERIAL2)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    #elif defined(ULISP_SERIAL1)
    else if (address == 1) pfun = serial1write;
    #endif
  }
  else if (streamtype == STRINGSTREAM) {
    pfun = pstr;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  #if defined(gfxsupport)
  else if (streamtype == GFXSTREAM) pfun = (pfun_t)gfxwrite;
  #endif
  #if defined(ULISP_WIFI)
  else if (streamtype == WIFISTREAM) pfun = (pfun_t)WiFiwrite;
  #endif
  else error2(NIL, PSTR("unknown stream type"));
  return pfun;
}

// Check pins - these are board-specific not processor-specific

void checkanalogread (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!(pin>=54 && pin<=65)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!(pin>=15 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!(pin>=14 && pin<=25)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin>=8 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_QTPY_M0)
  if (!((pin>=0 && pin<=3) || (pin>=6 && pin<=10))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SEEED_XIAO_M0)
  if (!(pin>=0 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=67 && pin<=74) || (pin>=54 && pin<=61)))  error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==29)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin==1 || pin==2 || (pin>=4 && pin<=6) || pin==21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==12 || pin==16)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!(pin>=49 && pin<=52)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=14 && pin<=27))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=14 && pin<=27) || (pin>=38 && pin<=41))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=26 && pin<=29)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!((pin>=2 && pin<=13) || pin==66 || pin==67)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || pin==14)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!((pin>=0 && pin<=8) || pin==10 || pin==18 || pin==19)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || (pin>=15 && pin<=16) || (pin>=22 && pin<=25))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin==0 || pin==2 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_QTPY_M0)
  if (!(pin==0 || (pin>=2 && pin<=10))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SEEED_XIAO_M0)
  if (!(pin>=0 && pin<=10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=0 && pin<=15)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin==0 || pin==1 || pin==4 || pin==5 || pin==7 || (pin>=9 && pin<=15) || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin==0 || pin==1 || (pin>=4 && pin<=6) || (pin>=9 && pin<=13) || pin==14 || pin==15 || pin==17 || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=2 && pin<=9) || pin==11 || (pin>=13 && pin<=45) || pin==48 || (pin>=50 && pin<=53) || pin==58 || pin==61 || pin==68 || pin==69)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin>=0 && pin<=30)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=0 && pin<=25)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!(pin>=0 && pin<=46)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!((pin>=20 && pin<=29) || pin==32 || (pin>=40 && pin<=48))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || (pin>=33 && pin<=39))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || pin==33 || (pin>=36 && pin<=37))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=0 && pin<=29)) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}

// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  if (!(pin>=26 && pin<=29)) error(ANALOGREAD, invalidpin, number(pin));
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
#else
  (void) pin, (void) note, (void) octave;
#endif
}

void nonote (int pin) {
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  noTone(pin);
#else
  (void) pin;
#endif
}

// Sleep

#if defined(CPU_ATSAMD21)
void WDT_Handler(void) {
  // ISR for watchdog early warning
  WDT->CTRL.bit.ENABLE = 0;        // Disable watchdog
  while(WDT->STATUS.bit.SYNCBUSY); // Sync CTRL write
  WDT->INTFLAG.bit.EW  = 1;        // Clear interrupt flag
}
#endif

void initsleep () {
#if defined(CPU_ATSAMD21)
 // One-time initialization of watchdog timer.

  // Generic clock generator 2, divisor = 32 (2^(DIV+1))
  GCLK->GENDIV.reg = GCLK_GENDIV_ID(2) | GCLK_GENDIV_DIV(4);
  // Enable clock generator 2 using low-power 32KHz oscillator.
  // With /32 divisor above, this yields 1024Hz clock.
  GCLK->GENCTRL.reg = GCLK_GENCTRL_ID(2) |
                      GCLK_GENCTRL_GENEN |
                      GCLK_GENCTRL_SRC_OSCULP32K |
                      GCLK_GENCTRL_DIVSEL;
  while(GCLK->STATUS.bit.SYNCBUSY);
  // WDT clock = clock gen 2
  GCLK->CLKCTRL.reg = GCLK_CLKCTRL_ID_WDT |
                      GCLK_CLKCTRL_CLKEN |
                      GCLK_CLKCTRL_GEN_GCLK2;

  // Enable WDT early-warning interrupt
  NVIC_DisableIRQ(WDT_IRQn);
  NVIC_ClearPendingIRQ(WDT_IRQn);
  NVIC_SetPriority(WDT_IRQn, 0);         // Top priority
  NVIC_EnableIRQ(WDT_IRQn);
#endif
}

void sleep (int secs) {
#if defined(CPU_ATSAMD21)
  WDT->CTRL.reg = 0;                     // Disable watchdog for config
  while(WDT->STATUS.bit.SYNCBUSY);
  WDT->INTENSET.bit.EW   = 1;            // Enable early warning interrupt
  WDT->CONFIG.bit.PER    = 0xB;          // Period = max
  WDT->CONFIG.bit.WINDOW = 0x7;          // Set time of interrupt = 1024 cycles = 1 sec
  WDT->CTRL.bit.WEN      = 1;            // Enable window mode
  while(WDT->STATUS.bit.SYNCBUSY);       // Sync CTRL write

  SysTick->CTRL = 0;                     // Stop SysTick interrupts

  while (secs > 0) {
    WDT->CLEAR.reg = WDT_CLEAR_CLEAR_KEY;// Clear watchdog interval
    while(WDT->STATUS.bit.SYNCBUSY);
    WDT->CTRL.bit.ENABLE = 1;            // Start watchdog now!
    while(WDT->STATUS.bit.SYNCBUSY);
    SCB->SCR |= SCB_SCR_SLEEPDEEP_Msk;   // Deepest sleep
    __DSB();
    __WFI();                             // Wait for interrupt
    secs--;
  }
  SysTick->CTRL = 7;                     // Restart SysTick interrupts
#else
  delay(1000*secs);
#endif
}

// Prettyprint

const int PPINDENT = 2;
const int PPWIDTH = 80;
const int GFXPPWIDTH = 52; // 320 pixel wide screen
int ppwidth = PPWIDTH;

void pcount (char c) {
  if (c == '\n') PrintCount++;
  PrintCount++;
}

uint8_t atomwidth (object *obj) {
  PrintCount = 0;
  printobject(obj, pcount);
  return PrintCount;
}

uint8_t basewidth (object *obj, uint8_t base) {
  PrintCount = 0;
  pintbase(obj->integer, base, pcount);
  return PrintCount;
}

bool quoted (object *obj) {
  return (consp(obj) && car(obj) != NULL && car(obj)->name == sym(QUOTE) && consp(cdr(obj)) && cddr(obj) == NULL);
}

int subwidth (object *obj, int w) {
  if (atom(obj)) return w - atomwidth(obj);
  if (quoted(obj)) obj = car(cdr(obj));
  return subwidthlist(obj, w - 1);
}

int subwidthlist (object *form, int w) {
  while (form != NULL && w >= 0) {
    if (atom(form)) return w - (2 + atomwidth(form));
    w = subwidth(car(form), w - 1);
    form = cdr(form);
  }
  return w;
}

void superprint (object *form, int lm, pfun_t pfun) {
  if (atom(form)) {
    if (symbolp(form) && form->name == sym(NOTHING)) printsymbol(form, pfun);
    else printobject(form, pfun);
  }
  else if (quoted(form)) { pfun('\''); superprint(car(cdr(form)), lm + 1, pfun); }
  else if (subwidth(form, ppwidth - lm) >= 0) supersub(form, lm + PPINDENT, 0, pfun);
  else supersub(form, lm + PPINDENT, 1, pfun);
}

const int ppspecials = 23;
const char ppspecial[ppspecials] PROGMEM =
  { DOTIMES, DOLIST, IF, SETQ, TEE, LET, LETSTAR, LAMBDA, WHEN, UNLESS, WITHI2C, WITHSERIAL, WITHSPI, WITHSDCARD, FORMILLIS,
    WITHOUTPUTTOSTRING, DEFVAR, CASE, WITHGFX, WITHCLIENT, UNWINDPROTECT, IGNOREERRORS, SP_ERROR };

void supersub (object *form, int lm, int super, pfun_t pfun) {
  int special = 0, separate = 1;
  object *arg = car(form);
  if (symbolp(arg)) {
    symbol_t sname = arg->name;
    if (sname == sym(DEFUN) || sname == sym(DEFCODE)) special = 2;
    else for (int i=0; i<ppspecials; i++) {
      if (sname == sym((builtin_t)ppspecial[i])) { special = 1; break; }
    }
  }
  while (form != NULL) {
    if (atom(form)) { pfstring(PSTR(" . "), pfun); printobject(form, pfun); pfun(')'); return; }
    else if (separate) { pfun('('); separate = 0; }
    else if (special) { pfun(' '); special--; }
    else if (!super) pfun(' ');
    else { pln(pfun); indent(lm, ' ', pfun); }
    superprint(car(form), lm, pfun);
    form = cdr(form);
  }
  pfun(')'); return;
}

object *edit (object *fun) {
  while (1) {
    if (tstflag(EXITEDITOR)) return fun;
    char c = gserial();
    if (c == 'q') setflag(EXITEDITOR);
    else if (c == 'b') return fun;
    else if (c == 'r') fun = read(gserial);
    else if (c == '\n') { pfl(pserial); superprint(fun, 0, pserial); pln(pserial); }
    else if (c == 'c') fun = cons(read(gserial), fun);
    else if (atom(fun)) pserial('!');
    else if (c == 'd') fun = cons(car(fun), edit(cdr(fun)));
    else if (c == 'a') fun = cons(edit(car(fun)), cdr(fun));
    else if (c == 'x') fun = cdr(fun);
    else pserial('?');
  }
}

// Assembler

object *call (int entry, int nargs, object *args, object *env) {
#if defined(CODESIZE)
  (void) env;
  int param[4];
  for (int i=0; i<nargs; i++) {
    object *arg = first(args);
    if (integerp(arg)) param[i] = arg->integer;
    else param[i] = (uintptr_t)arg;
    args = cdr(args);
  }
  int w = ((intfn_ptr_type)&MyCode[entry])(param[0], param[1], param[2], param[3]);
  return number(w);
#else
  return nil;
#endif
}

void putcode (object *arg, int origin, int pc) {
#if defined(CODESIZE)
  int code = checkinteger(DEFCODE, arg);
  MyCode[origin+pc] = code & 0xff;
  MyCode[origin+pc+1] = (code>>8) & 0xff;
  #if defined(assemblerlist)
  printhex4(pc, pserial);
  printhex4(code, pserial);
  #endif
#endif
}

int assemble (int pass, int origin, object *entries, object *env, object *pcpair) {
  int pc = 0; cdr(pcpair) = number(pc);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      if (pass == 2) {
        #if defined(assemblerlist)
        printhex4(pc, pserial);
        indent(5, ' ', pserial);
        printobject(arg, pserial); pln(pserial);
        #endif
      } else {
        object *pair = findvalue(DEFCODE, arg, env);
        cdr(pair) = number(pc);
      }
    } else {
      object *argval = eval(arg, env);
      if (listp(argval)) {
        object *arglist = argval;
        while (arglist != NULL) {
          if (pass == 2) {
            putcode(first(arglist), origin, pc);
            #if defined(assemblerlist)
            if (arglist == argval) superprint(arg, 0, pserial);
            pln(pserial);
            #endif
          }
          pc = pc + 2;
          cdr(pcpair) = number(pc);
          arglist = cdr(arglist);
        }
      } else if (integerp(argval)) {
        if (pass == 2) {
          putcode(argval, origin, pc);
          #if defined(assemblerlist)
          superprint(arg, 0, pserial); pln(pserial);
          #endif
        }
        pc = pc + 2;
        cdr(pcpair) = number(pc);
      } else error(DEFCODE, PSTR("illegal entry"), arg);
    }
    entries = cdr(entries);
  }
  // Round up to multiple of 4 to give code size
  if (pc%4 != 0) pc = pc + 4 - pc%4;
  return pc;
}

// Special forms

object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, args);
  return first(args);
}

object *sp_or (object *args, object *env) {
  while (args != NULL) {
    object *val = eval(car(args), env);
    if (val != NULL) return val;
    args = cdr(args);
  }
  return nil;
}

object *sp_defun (object *args, object *env) {
  (void) env;
  checkargs(DEFUN, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFUN, notasymbol, var);
  object *val = cons(bsymbol(LAMBDA), cdr(args));
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}

object *sp_defvar (object *args, object *env) {
  checkargs(DEFVAR, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFVAR, notasymbol, var);
  object *val = NULL;
  args = cdr(args);
  if (args != NULL) { setflag(NOESC); val = eval(first(args), env); clrflag(NOESC); }
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}

object *sp_setq (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETQ, oddargs);
    object *pair = findvalue(SETQ, first(args), env);
    arg = eval(second(args), env);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}

object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    testescape();
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
  }
}

object *sp_return (object *args, object *env) {
  object *result = eval(tf_progn(args,env), env);
  setflag(RETURNFLAG);
  return result;
}

object *sp_push (object *args, object *env) {
  int bit;
  checkargs(PUSH, args);
  object *item = eval(first(args), env);
  object **loc = place(PUSH, second(args), env, &bit);
  push(item, *loc);
  return *loc;
}

object *sp_pop (object *args, object *env) {
  int bit;
  checkargs(POP, args);
  object **loc = place(POP, first(args), env, &bit);
  object *result = car(*loc);
  pop(*loc);
  return result;
}

// Accessors

object *sp_incf (object *args, object *env) {
  int bit;
  checkargs(INCF, args);
  object **loc = place(INCF, first(args), env, &bit);
  args = cdr(args);

  object *x = *loc;
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int increment;
    if (inc == NULL) increment = 1; else increment = checkbitvalue(INCF, inc);
    int newvalue = (((*loc)->integer)>>bit & 1) + increment;

    if (newvalue & ~1) error2(INCF, PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (floatp(x) || floatp(inc)) {
    float increment;
    float value = checkintfloat(INCF, x);

    if (inc == NULL) increment = 1.0; else increment = checkintfloat(INCF, inc);

    *loc = makefloat(value + increment);
  } else if (integerp(x) && (integerp(inc) || inc == NULL)) {
    int increment;
    int value = x->integer;

    if (inc == NULL) increment = 1; else increment = inc->integer;

    if (increment < 1) {
      if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    } else {
      if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    }
  } else error2(INCF, notanumber);
  return *loc;
}

object *sp_decf (object *args, object *env) {
  int bit;
  checkargs(DECF, args);
  object **loc = place(DECF, first(args), env, &bit);
  args = cdr(args);

  object *x = *loc;
  object *dec = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int decrement;
    if (dec == NULL) decrement = 1; else decrement = checkbitvalue(DECF, dec);
    int newvalue = (((*loc)->integer)>>bit & 1) - decrement;

    if (newvalue & ~1) error2(INCF, PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (floatp(x) || floatp(dec)) {
    float decrement;
    float value = checkintfloat(DECF, x);

    if (dec == NULL) decrement = 1.0; else decrement = checkintfloat(DECF, dec);

    *loc = makefloat(value - decrement);
  } if (integerp(x) && (integerp(dec) || dec == NULL)) {
    int decrement;
    int value = x->integer;

    if (dec == NULL) decrement = 1; else decrement = dec->integer;

    if (decrement < 1) {
      if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    } else {
      if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    }
  } else error2(DECF, notanumber);
  return *loc;
}

object *sp_setf (object *args, object *env) {
  int bit;
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, oddargs);
    object **loc = place(SETF, first(args), env, &bit);
    arg = eval(second(args), env);
    if (bit == -1) *loc = arg;
    else *loc = number((checkinteger(SETF,*loc) & ~(1<<bit)) | checkbitvalue(SETF,arg)<<bit);
    args = cddr(args);
  }
  return arg;
}

// Other special forms

object *sp_dolist (object *args, object *env) {
  if (args == NULL || listlength(DOLIST, first(args)) < 2) error2(DOLIST, noargument);
  object *params = first(args);
  object *var = first(params);
  object *list = eval(second(params), env);
  push(list, GCStack); // Don't GC the list
  object *pair = cons(var,nil);
  push(pair,env);
  params = cdr(cdr(params));
  args = cdr(args);
  while (list != NULL) {
    if (improperp(list)) error(DOLIST, notproper, list);
    cdr(pair) = first(list);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        pop(GCStack);
        return result;
      }
      forms = cdr(forms);
    }
    list = cdr(list);
  }
  cdr(pair) = nil;
  pop(GCStack);
  if (params == NULL) return nil;
  return eval(car(params), env);
}

object *sp_dotimes (object *args, object *env) {
  if (args == NULL || listlength(DOTIMES, first(args)) < 2) error2(DOTIMES, noargument);
  object *params = first(args);
  object *var = first(params);
  int count = checkinteger(DOTIMES, eval(second(params), env));
  int index = 0;
  params = cdr(cdr(params));
  object *pair = cons(var,number(0));
  push(pair,env);
  args = cdr(args);
  while (index < count) {
    cdr(pair) = number(index);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      forms = cdr(forms);
    }
    index++;
  }
  cdr(pair) = number(index);
  if (params == NULL) return nil;
  return eval(car(params), env);
}

object *sp_trace (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *var = first(args);
    if (!symbolp(var)) error(TRACE, notasymbol, var);
    trace(var->name);
    args = cdr(args);
  }
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
    i++;
  }
  return args;
}

object *sp_untrace (object *args, object *env) {
  (void) env;
  if (args == NULL) {
    int i = 0;
    while (i < TRACEMAX) {
      if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
      TraceFn[i] = 0;
      i++;
    }
  } else {
    while (args != NULL) {
      object *var = first(args);
      if (!symbolp(var)) error(UNTRACE, notasymbol, var);
      untrace(var->name);
      args = cdr(args);
    }
  }
  return args;
}

object *sp_formillis (object *args, object *env) {
  if (args == NULL) error2(FORMILLIS, noargument);
  object *param = first(args);
  unsigned long start = millis();
  unsigned long now, total = 0;
  if (param != NULL) total = checkinteger(FORMILLIS, eval(first(param), env));
  eval(tf_progn(cdr(args),env), env);
  do {
    now = millis() - start;
    testescape();
  } while (now < total);
  if (now <= INT_MAX) return number(now);
  return nil;
}

object *sp_time (object *args, object *env) {
  unsigned long start = millis();
  object *result = eval(first(args), env);
  unsigned long elapsed = millis() - start;
  printobject(result, pserial);
  pfstring(PSTR("\nTime: "), pserial);
  if (elapsed < 1000) {
    pint(elapsed, pserial);
    pfstring(PSTR(" ms\n"), pserial);
  } else {
    elapsed = elapsed+50;
    pint(elapsed/1000, pserial);
    pserial('.'); pint((elapsed/100)%10, pserial);
    pfstring(PSTR(" s\n"), pserial);
  }
  return bsymbol(NOTHING);
}

object *sp_withoutputtostring (object *args, object *env) {
  if (args == NULL) error2(WITHOUTPUTTOSTRING, noargument);
  object *params = first(args);
  if (params == NULL) error2(WITHOUTPUTTOSTRING, nostream);
  object *var = first(params);
  object *pair = cons(var, stream(STRINGSTREAM, 0));
  push(pair,env);
  object *string = startstring(WITHOUTPUTTOSTRING);
  push(string, GCStack);
  object *forms = cdr(args);
  eval(tf_progn(forms,env), env);
  pop(GCStack);
  return string;
}

object *sp_withserial (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSERIAL, nostream);
  object *var = first(params);
  int address = checkinteger(WITHSERIAL, eval(second(params), env));
  params = cddr(params);
  int baud = 96;
  if (params != NULL) baud = checkinteger(WITHSERIAL, eval(first(params), env));
  object *pair = cons(var, stream(SERIALSTREAM, address));
  push(pair,env);
  serialbegin(address, baud);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  serialend(address);
  return result;
}

object *sp_withi2c (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHI2C, nostream);
  object *var = first(params);
  int address = checkinteger(WITHI2C, eval(second(params), env));
  params = cddr(params);
  if ((address == 0 || address == 1) && params != NULL) {
    address = address * 128 + checkinteger(WITHI2C, eval(first(params), env));
    params = cdr(params);
  }
  int read = 0; // Write
  I2Ccount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  // Top bit of address is I2C port
  TwoWire *port = &Wire;
  #if defined(ULISP_I2C1)
  if (address > 127) port = &Wire1;
  #endif
  I2Cinit(port, 1); // Pullups
  object *pair = cons(var, (I2Cstart(port, address & 0x7F, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(port, read);
  return result;
}

object *sp_withspi (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSPI, nostream);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(WITHSPI, nostream);
  int pin = checkinteger(WITHSPI, eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0, address = 0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(WITHSPI, eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(WITHSPI, eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(WITHSPI, eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
        params = cdr(params);
        if (params != NULL) {
          address = checkinteger(WITHSPI, eval(car(params), env));
        }
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin + 128*address));
  push(pair,env);
  SPIClass *spiClass = &mySPI0;
  #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
  if (address == 1) spiClass = &SPI1;
  #endif
  mySPI0.begin();
  mySPI0.beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  mySPI0.endTransaction();
  mySPI0.end();
  return result;
}

object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = first(args);
  if (params == NULL) error2(WITHSDCARD, nostream);
  object *var = first(params);
  object *filename = eval(second(params), env);
  params = cddr(params);

  // enable power to SD card
  sd_on();

  SD.begin(SDCARD_SS_PIN);
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(WITHSDCARD, first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    char buffer[BUFFERSIZE];
    SDpfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDpfile) error2(WITHSDCARD, PSTR("problem writing to SD card or invalid filename"));
  } else {
    char buffer[BUFFERSIZE];
    SDgfile = SD.open(MakeFilename(filename, buffer), oflag);
    if (!SDgfile) error2(WITHSDCARD, PSTR("problem reading from SD card or invalid filename"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  SD.end();

  sd_off();

  return result;
  #else
  (void) args, (void) env;
  error2(WITHSDCARD, PSTR("not supported"));
  return nil;
  #endif
}

object *sp_withgfx (object *args, object *env) {
#if defined(gfxsupport)
  object *params = first(args);
  object *var = first(params);
  object *pair = cons(var, stream(GFXSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
#else
  (void) args, (void) env;
  error2(WITHGFX, PSTR("not supported"));
  return nil;
#endif
}

object *sp_withclient (object *args, object *env) {
  #if defined(ULISP_WIFI)
  object *params = first(args);
  object *var = first(params);
  char buffer[BUFFERSIZE];
  params = cdr(params);
  int n;
  if (params == NULL) {
    client = server.available();
    if (!client) return nil;
    n = 2;
  } else {
    object *address = eval(first(params), env);
    object *port = eval(second(params), env);
    int success;
    if (stringp(address)) success = client.connect(cstring(WITHCLIENT, address, buffer, BUFFERSIZE), checkinteger(WITHCLIENT, port));
    else if (integerp(address)) success = client.connect(address->integer, checkinteger(WITHCLIENT, port));
    else error2(WITHCLIENT, PSTR("invalid address"));
    if (!success) return nil;
    n = 1;
  }
  object *pair = cons(var, stream(WIFISTREAM, n));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  client.stop();
  return result;
  #else
  (void) args, (void) env;
  error2(WITHCLIENT, PSTR("not supported"));
  return nil;
  #endif
}

object *sp_unwindprotect (object *args, object *env) {
  checkargs(UNWINDPROTECT, args);
  object *current_GCStack = GCStack;
  jmp_buf dynamic_handler;
  jmp_buf *previous_handler = handler;
  handler = &dynamic_handler;
  object *protected_form = first(args);
  object *result;

  bool signaled = false;
  if (!setjmp(dynamic_handler)) {
    result = eval(protected_form, env);
  } else {
    GCStack = current_GCStack;
    signaled = true;
  }
  handler = previous_handler;

  object *protective_forms = cdr(args);
  while (protective_forms != NULL) {
    eval(car(protective_forms),env);
    if (tstflag(RETURNFLAG)) break;
    protective_forms = cdr(protective_forms);
  }

  if (signaled) {
    GCStack = NULL;
    longjmp(*handler, 1);
  }
  else return result;
}

object *sp_ignoreerrors (object *args, object *env) {
  checkargs(IGNOREERRORS, args);
  object *current_GCStack = GCStack;
  jmp_buf dynamic_handler;
  jmp_buf *previous_handler = handler;
  handler = &dynamic_handler;
  object *result = nil;

  bool muffled = tstflag(MUFFLEERRORS);
  setflag(MUFFLEERRORS);
  bool signaled = false;
  if (!setjmp(dynamic_handler)) {
    while (args != NULL) {
      result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) break;
      args = cdr(args);
    }
  } else {
    GCStack = current_GCStack;
    signaled = true;
  }
  handler = previous_handler;
  if (!muffled) clrflag(MUFFLEERRORS);

  if (signaled) return symbol(NOTHING);
  else return result;
}

object *sp_error (object *args, object *env) {
  // If called with no arguments, clear the error string.
  if (args == NULL) {
    GlobalErrorString = NULL;
    return NULL;
  }

  object *arg = eval(car(args),env); // eval first arg
  args = cdr(args);

  object *message;
  if (symbolp(arg)) {
    error2(SP_ERROR, PSTR("Error must be a (format) string"));
  } else { // just let FORMAT handle it
    message = eval(
      cons(bsymbol(FORMAT), cons(nil, cons(arg, args))),
      env);
  }

  // store error message in GlobalErrorString for GET-ERROR
  GlobalErrorString = message;

  if (!tstflag(MUFFLEERRORS)) {
    char temp = Flags;
    clrflag(PRINTREADABLY);
    pfstring(PSTR("SP Error: "), pserial); printstring(message, pserial);
    Flags = temp;
    pln(pserial);
  }
  GCStack = NULL;
  longjmp(*handler, 1);
}

object *fn_geterror (object *args, object *env) {
  return GlobalErrorString;
}

// Assembler

object *sp_defcode (object *args, object *env) {
#if defined(CODESIZE)
  setflag(NOESC);
  checkargs(DEFCODE, args);
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(DEFCODE, PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers r0, r1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(DEFCODE, PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), bsymbol((builtin_t)((toradix40('r')*40+toradix40('0')+regn)*2560000))); // Symbol for r0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }

  // Make *pc* a local variable for program counter
  object *pcpair = cons(bsymbol(PSTAR), number(0));
  push(pcpair,env);

  args = cdr(args);

  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(DEFCODE, PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }        
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }

  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
#else
  error2(DEFCODE, PSTR("not available"));
  return nil;
#endif
}

// Tail-recursive forms

object *tf_progn (object *args, object *env) {
  if (args == NULL) return nil;
  object *more = cdr(args);
  while (more != NULL) {
    object *result = eval(car(args),env);
    if (tstflag(RETURNFLAG)) return result;
    args = more;
    more = cdr(args);
  }
  return car(args);
}

object *tf_if (object *args, object *env) {
  if (args == NULL || cdr(args) == NULL) error2(IF, toofewargs);
  if (eval(first(args), env) != nil) return second(args);
  args = cddr(args);
  return (args != NULL) ? first(args) : nil;
}

object *tf_cond (object *args, object *env) {
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(COND, illegalclause, clause);
    object *test = eval(first(clause), env);
    object *forms = cdr(clause);
    if (test != nil) {
      if (forms == NULL) return quote(test); else return tf_progn(forms, env);
    }
    args = cdr(args);
  }
  return nil;
}

object *tf_when (object *args, object *env) {
  if (args == NULL) error2(WHEN, noargument);
  if (eval(first(args), env) != nil) return tf_progn(cdr(args),env);
  else return nil;
}

object *tf_unless (object *args, object *env) {
  if (args == NULL) error2(UNLESS, noargument);
  if (eval(first(args), env) != nil) return nil;
  else return tf_progn(cdr(args),env);
}

object *tf_case (object *args, object *env) {
  object *test = eval(first(args), env);
  args = cdr(args);
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(CASE, illegalclause, clause);
    object *key = car(clause);
    object *forms = cdr(clause);
    if (consp(key)) {
      while (key != NULL) {
        if (eq(test,car(key))) return tf_progn(forms, env);
        key = cdr(key);
      }
    } else if (eq(test,key) || eq(key,tee)) return tf_progn(forms, env);
    args = cdr(args);
  }
  return nil;
}

object *tf_and (object *args, object *env) {
  if (args == NULL) return tee;
  object *more = cdr(args);
  while (more != NULL) {
    if (eval(car(args), env) == NULL) return nil;
    args = more;
    more = cdr(args);
  }
  return car(args);
}

object *tf_help (object *args, object *env) {
  if (args == NULL) error2(HELP, noargument);
  object *docstring = documentation(HELP, first(args), env);
  if (docstring) {
    char temp = Flags;
    clrflag(PRINTREADABLY);
    printstring(docstring, pserial);
    Flags = temp;
  }
  return bsymbol(NOTHING);
}

// Core functions

object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}

object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}

object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}

object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}

object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}

object *fn_symbolp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (arg == NULL || symbolp(arg)) ? tee : nil;
}

object *fn_arrayp (object *args, object *env) {
  (void) env;
  return arrayp(first(args)) ? tee : nil;
}

object *fn_boundp (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(BOUNDP, notasymbol, var);
  return boundp(var, env) ? tee : nil;
}

object *fn_setfn (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETFN, oddargs);
    object *pair = findvalue(SETFN, first(args), env);
    arg = second(args);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}

object *fn_streamp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return streamp(arg) ? tee : nil;
}

object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}

// List functions

object *fn_car (object *args, object *env) {
  (void) env;
  return carx(first(args));
}

object *fn_cdr (object *args, object *env) {
  (void) env;
  return cdrx(first(args));
}

object *fn_caar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b100);
}

object *fn_cadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b101);
}

object *fn_cdar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b110);
}

object *fn_cddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b111);
}

object *fn_caaar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1000);
}

object *fn_caadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1001);;
}

object *fn_cadar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1010);
}

object *fn_caddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1011);
}

object *fn_cdaar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1100);
}

object *fn_cdadr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1101);
}

object *fn_cddar (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1110);
}

object *fn_cdddr (object *args, object *env) {
  (void) env;
  return cxxxr(args, 0b1111);
}

object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (stringp(arg)) return number(stringlength(arg));
  if (!(arrayp(arg) && cdr(cddr(arg)) == NULL)) error(LENGTH, PSTR("argument is not a list, 1d array, or string"), arg);
  return number(abs(first(cddr(arg))->integer));
}

object *fn_arraydimensions (object *args, object *env) {
  (void) env;
  object *array = first(args);
  if (!arrayp(array)) error(ARRAYDIMENSIONS, PSTR("argument is not an array"), array);
  object *dimensions = cddr(array);
  return (first(dimensions)->integer < 0) ? cons(number(-(first(dimensions)->integer)), cdr(dimensions)) : dimensions;
}

object *fn_list (object *args, object *env) {
  (void) env;
  return args;
}

object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  bool bitp = false;
  object *dims = first(args);
  if (dims == NULL) error2(MAKEARRAY, PSTR("dimensions can't be nil"));
  else if (atom(dims)) dims = cons(dims, NULL);
  args = cdr(args);
  while (args != NULL && cdr(args) != NULL) {
    object *var = first(args);
    if (isbuiltin(first(args), INITIALELEMENT)) def = second(args);
    else if (isbuiltin(first(args), ELEMENTTYPE) && isbuiltin(second(args), BIT)) bitp = true;
    else error(MAKEARRAY, PSTR("argument not recognised"), var);
    args = cddr(args);
  }
  if (bitp) {
    if (def == nil) def = number(0);
    else def = number(-checkbitvalue(MAKEARRAY, def)); // 1 becomes all ones
  }
  return makearray(MAKEARRAY, dims, def, bitp);
}

object *fn_reverse (object *args, object *env) {
  (void) env;
  object *list = first(args);
  object *result = NULL;
  while (list != NULL) {
    if (improperp(list)) error(REVERSE, notproper, list);
    push(first(list),result);
    list = cdr(list);
  }
  return result;
}

object *fn_nth (object *args, object *env) {
  (void) env;
  int n = checkinteger(NTH, first(args));
  if (n < 0) error(NTH, indexnegative, first(args));
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(NTH, notproper, list);
    if (n == 0) return car(list);
    list = cdr(list);
    n--;
  }
  return nil;
}

object *fn_aref (object *args, object *env) {
  (void) env;
  int bit;
  object *array = first(args);
  if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
  object *loc = *getarray(AREF, array, cdr(args), 0, &bit);
  if (bit == -1) return loc;
  else return number((loc->integer)>>bit & 1);
}

object *fn_assoc (object *args, object *env) {
  (void) env;
  object *key = first(args);
  object *list = second(args);
  return assoc(key,list);
}

object *fn_member (object *args, object *env) {
  (void) env;
  object *item = first(args);
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(MEMBER, notproper, list);
    if (eq(item,car(list))) return list;
    list = cdr(list);
  }
  return nil;
}

object *fn_apply (object *args, object *env) {
  object *previous = NULL;
  object *last = args;
  while (cdr(last) != NULL) {
    previous = last;
    last = cdr(last);
  }
  object *arg = car(last);
  if (!listp(arg)) error(APPLY, notalist, arg);
  cdr(previous) = arg;
  return apply(APPLY, first(args), cdr(args), env);
}

object *fn_funcall (object *args, object *env) {
  return apply(FUNCALL, first(args), cdr(args), env);
}

object *fn_append (object *args, object *env) {
  (void) env;
  object *head = NULL;
  object *tail;
  while (args != NULL) {
    object *list = first(args);
    if (!listp(list)) error(APPEND, notalist, list);
    while (consp(list)) {
      object *obj = cons(car(list), cdr(list));
      if (head == NULL) head = obj;
      else cdr(tail) = obj;
      tail = obj;
      list = cdr(list);
      if (cdr(args) != NULL && improperp(list)) error(APPEND, notproper, first(args));
    }
    args = cdr(args);
  }
  return head;
}

object *fn_mapc (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *result = first(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         return result;
      }
      if (improperp(list)) error(MAPC, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    apply(MAPC, function, cdr(params), env);
  }
}

object *fn_mapcar (object *args, object *env) {
  return mapcarcan(MAPCAR, args, env, mapcarfun);
}

object *fn_mapcan (object *args, object *env) {
  return mapcarcan(MAPCAN, args, env, mapcanfun);
}

// Arithmetic functions

object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    object *arg = car(args);
    if (floatp(arg)) return add_floats(args, (float)result);
    else if (integerp(arg)) {
      int val = arg->integer;
      if (val < 1) { if (INT_MIN - val > result) return add_floats(args, (float)result); }
      else { if (INT_MAX - val < result) return add_floats(args, (float)result); }
      result = result + val;
    } else error(ADD, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}

object *fn_subtract (object *args, object *env) {
  (void) env;
  object *arg = car(args);
  args = cdr(args);
  if (args == NULL) return negate(arg);
  else if (floatp(arg)) return subtract_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) return subtract_floats(args, result);
      else if (integerp(arg)) {
        int val = (car(args))->integer;
        if (val < 1) { if (INT_MAX + val < result) return subtract_floats(args, result); }
        else { if (INT_MIN + val > result) return subtract_floats(args, result); }
        result = result - val;
      } else error(SUBTRACT, notanumber, arg);
      args = cdr(args);
    }
    return number(result);
  } else error(SUBTRACT, notanumber, arg);
  return nil;
}

object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    object *arg = car(args);
    if (floatp(arg)) return multiply_floats(args, result);
    else if (integerp(arg)) {
      int64_t val = result * (int64_t)(arg->integer);
      if ((val > INT_MAX) || (val < INT_MIN)) return multiply_floats(args, result);
      result = val;
    } else error(MULTIPLY, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}

object *fn_divide (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  args = cdr(args);
  // One argument
  if (args == NULL) {
    if (floatp(arg)) {
      float f = arg->single_float;
      if (f == 0.0) error2(DIVIDE, PSTR("division by zero"));
      return makefloat(1.0 / f);
    } else if (integerp(arg)) {
      int i = arg->integer;
      if (i == 0) error2(DIVIDE, PSTR("division by zero"));
      else if (i == 1) return number(1);
      else return makefloat(1.0 / i);
    } else error(DIVIDE, notanumber, arg);
  }
  // Multiple arguments
  if (floatp(arg)) return divide_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) {
        return divide_floats(args, result);
      } else if (integerp(arg)) {
        int i = arg->integer;
        if (i == 0) error2(DIVIDE, PSTR("division by zero"));
        if ((result % i) != 0) return divide_floats(args, result);
        if ((result == INT_MIN) && (i == -1)) return divide_floats(args, result);
        result = result / i;
        args = cdr(args);
      } else error(DIVIDE, notanumber, arg);
    }
    return number(result);
  } else error(DIVIDE, notanumber, arg);
  return nil;
}

object *fn_mod (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  object *arg2 = second(args);
  if (integerp(arg1) && integerp(arg2)) {
    int divisor = arg2->integer;
    if (divisor == 0) error2(MOD, PSTR("division by zero"));
    int dividend = arg1->integer;
    int remainder = dividend % divisor;
    if ((dividend<0) != (divisor<0)) remainder = remainder + divisor;
    return number(remainder);
  } else {
    float fdivisor = checkintfloat(MOD, arg2);
    if (fdivisor == 0.0) error2(MOD, PSTR("division by zero"));
    float fdividend = checkintfloat(MOD, arg1);
    float fremainder = fmod(fdividend , fdivisor);
    if ((fdividend<0) != (fdivisor<0)) fremainder = fremainder + fdivisor;
    return makefloat(fremainder);
  }
}

object *fn_oneplus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) + 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MAX) return makefloat((arg->integer) + 1.0);
    else return number(result + 1);
  } else error(ONEPLUS, notanumber, arg);
  return nil;
}

object *fn_oneminus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) - 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat((arg->integer) - 1.0);
    else return number(result - 1);
  } else error(ONEMINUS, notanumber, arg);
  return nil;
}

object *fn_abs (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return makefloat(abs(arg->single_float));
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(abs((float)result));
    else return number(abs(result));
  } else error(ABS, notanumber, arg);
  return nil;
}

object *fn_random (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (integerp(arg)) return number(random(arg->integer));
  else if (floatp(arg)) return makefloat((float)rand()/(float)(RAND_MAX/(arg->single_float)));
  else error(RANDOM, notanumber, arg);
  return nil;
}

object *fn_maxfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) > (result->integer)) result = arg;
    } else if ((checkintfloat(MAXFN, arg) > checkintfloat(MAXFN, result))) result = arg;
    args = cdr(args);
  }
  return result;
}

object *fn_minfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) < (result->integer)) result = arg;
    } else if ((checkintfloat(MINFN, arg) < checkintfloat(MINFN, result))) result = arg;
    args = cdr(args);
  }
  return result;
}

// Arithmetic comparisons

object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *nargs = args;
    object *arg1 = first(nargs);
    nargs = cdr(nargs);
    while (nargs != NULL) {
      object *arg2 = first(nargs);
      if (integerp(arg1) && integerp(arg2)) {
        if ((arg1->integer) == (arg2->integer)) return nil;
      } else if ((checkintfloat(NOTEQ, arg1) == checkintfloat(NOTEQ, arg2))) return nil;
      nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}

object *fn_numeq (object *args, object *env) {
  (void) env;
  return compare(NUMEQ, args, false, false, true);
}

object *fn_less (object *args, object *env) {
  (void) env;
  return compare(LESS, args, true, false, false);
}

object *fn_lesseq (object *args, object *env) {
  (void) env;
  return compare(LESSEQ, args, true, false, true);
}

object *fn_greater (object *args, object *env) {
  (void) env;
  return compare(GREATER, args, false, true, false);
}

object *fn_greatereq (object *args, object *env) {
  (void) env;
  return compare(GREATEREQ, args, false, true, true);
}

object *fn_plusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) > 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) > 0) ? tee : nil;
  else error(PLUSP, notanumber, arg);
  return nil;
}

object *fn_minusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) < 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) < 0) ? tee : nil;
  else error(MINUSP, notanumber, arg);
  return nil;
}

object *fn_zerop (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) == 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) == 0) ? tee : nil;
  else error(ZEROP, notanumber, arg);
  return nil;
}

object *fn_oddp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ODDP, first(args));
  return ((arg & 1) == 1) ? tee : nil;
}

object *fn_evenp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(EVENP, first(args));
  return ((arg & 1) == 0) ? tee : nil;
}

// Number functions

object *fn_integerp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}

object *fn_numberp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (integerp(arg) || floatp(arg)) ? tee : nil;
}

// Floating-point functions

object *fn_floatfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (floatp(arg)) ? arg : makefloat((float)(arg->integer));
}

object *fn_floatp (object *args, object *env) {
  (void) env;
  return floatp(first(args)) ? tee : nil;
}

object *fn_sin (object *args, object *env) {
  (void) env;
  return makefloat(sin(checkintfloat(SIN, first(args))));
}

object *fn_cos (object *args, object *env) {
  (void) env;
  return makefloat(cos(checkintfloat(COS, first(args))));
}

object *fn_tan (object *args, object *env) {
  (void) env;
  return makefloat(tan(checkintfloat(TAN, first(args))));
}

object *fn_asin (object *args, object *env) {
  (void) env;
  return makefloat(asin(checkintfloat(ASIN, first(args))));
}

object *fn_acos (object *args, object *env) {
  (void) env;
  return makefloat(acos(checkintfloat(ACOS, first(args))));
}

object *fn_atan (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float div = 1.0;
  args = cdr(args);
  if (args != NULL) div = checkintfloat(ATAN, first(args));
  return makefloat(atan2(checkintfloat(ATAN, arg), div));
}

object *fn_sinh (object *args, object *env) {
  (void) env;
  return makefloat(sinh(checkintfloat(SINH, first(args))));
}

object *fn_cosh (object *args, object *env) {
  (void) env;
  return makefloat(cosh(checkintfloat(COSH, first(args))));
}

object *fn_tanh (object *args, object *env) {
  (void) env;
  return makefloat(tanh(checkintfloat(TANH, first(args))));
}

object *fn_exp (object *args, object *env) {
  (void) env;
  return makefloat(exp(checkintfloat(EXP, first(args))));
}

object *fn_sqrt (object *args, object *env) {
  (void) env;
  return makefloat(sqrt(checkintfloat(SQRT, first(args))));
}

object *fn_log (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float fresult = log(checkintfloat(LOG, arg));
  args = cdr(args);
  if (args == NULL) return makefloat(fresult);
  else return makefloat(fresult / log(checkintfloat(LOG, first(args))));
}

object *fn_expt (object *args, object *env) {
  (void) env;
  object *arg1 = first(args); object *arg2 = second(args);
  float float1 = checkintfloat(EXPT, arg1);
  float value = log(abs(float1)) * checkintfloat(EXPT, arg2);
  if (integerp(arg1) && integerp(arg2) && ((arg2->integer) >= 0) && (abs(value) < 21.4875))
    return number(intpower(arg1->integer, arg2->integer));
  if (float1 < 0) {
    if (integerp(arg2)) return makefloat((arg2->integer & 1) ? -exp(value) : exp(value));
    else error2(EXPT, PSTR("invalid result"));
  }
  return makefloat(exp(value));
}

object *fn_ceiling (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(ceil(checkintfloat(CEILING, arg) / checkintfloat(CEILING, first(args))));
  else return number(ceil(checkintfloat(CEILING, arg)));
}

object *fn_floor (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(floor(checkintfloat(FLOOR, arg) / checkintfloat(FLOOR, first(args))));
  else return number(floor(checkintfloat(FLOOR, arg)));
}

object *fn_truncate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number((int)(checkintfloat(TRUNCATE, arg) / checkintfloat(TRUNCATE, first(args))));
  else return number((int)(checkintfloat(TRUNCATE, arg)));
}

object *fn_round (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(myround(checkintfloat(ROUND, arg) / checkintfloat(ROUND, first(args))));
  else return number(myround(checkintfloat(ROUND, arg)));
}

// Characters

object *fn_char (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(CHAR, notastring, arg);
  object *n = second(args);
  char c = nthchar(arg, checkinteger(CHAR, n));
  if (c == 0) error(CHAR, indexrange, n);
  return character(c);
}

object *fn_charcode (object *args, object *env) {
  (void) env;
  return number(checkchar(CHARCODE, first(args)));
}

object *fn_codechar (object *args, object *env) {
  (void) env;
  return character(checkinteger(CODECHAR, first(args)));
}

object *fn_characterp (object *args, object *env) {
  (void) env;
  return characterp(first(args)) ? tee : nil;
}

// Strings

object *fn_stringp (object *args, object *env) {
  (void) env;
  return stringp(first(args)) ? tee : nil;
}

object *fn_stringeq (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGEQ, args, false, false, true) ? tee : nil;
}

object *fn_stringless (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGLESS, args, true, false, false) ? tee : nil;
}

object *fn_stringgreater (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGGREATER, args, false, true, false) ? tee : nil;
}

object *fn_sort (object *args, object *env) {
  if (first(args) == NULL) return nil;
  object *list = cons(nil,first(args));
  push(list,GCStack);
  object *predicate = second(args);
  object *compare = cons(NULL, cons(NULL, NULL));
  push(compare,GCStack);
  object *ptr = cdr(list);
  while (cdr(ptr) != NULL) {
    object *go = list;
    while (go != ptr) {
      car(compare) = car(cdr(ptr));
      car(cdr(compare)) = car(cdr(go));
      if (apply(SORT, predicate, compare, env)) break;
      go = cdr(go);
    }
    if (go != ptr) {
      object *obj = cdr(ptr);
      cdr(ptr) = cdr(obj);
      cdr(obj) = cdr(go);
      cdr(go) = obj;
    } else ptr = cdr(ptr);
  }
  pop(GCStack); pop(GCStack);
  return cdr(list);
}

object *fn_stringfn (object *args, object *env) {
  return fn_princtostring(args, env);
}

object *fn_concatenate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (builtin(arg->name) != STRINGFN) error2(CONCATENATE, PSTR("only supports strings"));
  args = cdr(args);
  object *result = newstring();
  object *tail = result;
  while (args != NULL) {
    object *obj = checkstring(CONCATENATE, first(args));
    obj = cdr(obj);
    while (obj != NULL) {
      int quad = obj->chars;
      while (quad != 0) {
         char ch = quad>>((sizeof(int)-1)*8) & 0xFF;
         buildstring(ch, &tail);
         quad = quad<<8;
      }
      obj = car(obj);
    }
    args = cdr(args);
  }
  return result;
}

object *fn_subseq (object *args, object *env) {
  (void) env;
  object *arg = checkstring(SUBSEQ, first(args));
  int start = checkinteger(SUBSEQ, second(args));
  if (start < 0) error(SUBSEQ, indexnegative, second(args));
  int end;
  args = cddr(args);
  if (args != NULL) end = checkinteger(SUBSEQ, car(args)); else end = stringlength(arg);
  object *result = newstring();
  object *tail = result;
  for (int i=start; i<end; i++) {
    char ch = nthchar(arg, i);
    if (ch == 0) error2(SUBSEQ, indexrange);
    buildstring(ch, &tail);
  }
  return result;
}

object *fn_readfromstring (object *args, object *env) {
  (void) env;
  object *arg = checkstring(READFROMSTRING, first(args));
  GlobalString = arg;
  GlobalStringIndex = 0;
  return read(gstr);
}

object *fn_princtostring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRINCTOSTRING);
  prin1object(arg, pstr);
  return obj;
}

object *fn_prin1tostring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRIN1TOSTRING);
  printobject(arg, pstr);
  return obj;
}

// Bitwise operators

object *fn_logand (object *args, object *env) {
  (void) env;
  int result = -1;
  while (args != NULL) {
    result = result & checkinteger(LOGAND, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_logior (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    result = result | checkinteger(LOGIOR, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_logxor (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    result = result ^ checkinteger(LOGXOR, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_lognot (object *args, object *env) {
  (void) env;
  int result = checkinteger(LOGNOT, car(args));
  return number(~result);
}

object *fn_ash (object *args, object *env) {
  (void) env;
  int value = checkinteger(ASH, first(args));
  int count = checkinteger(ASH, second(args));
  if (count >= 0) return number(value << count);
  else return number(value >> abs(count));
}

object *fn_logbitp (object *args, object *env) {
  (void) env;
  int index = checkinteger(LOGBITP, first(args));
  int value = checkinteger(LOGBITP, second(args));
  return (bitRead(value, index) == 1) ? tee : nil;
}

// System functions

object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}

object *fn_globals (object *args, object *env) {
  (void) args;
  if (GlobalEnv == NULL) return nil;
  return fn_mapcar(cons(bsymbol(CAR),cons(GlobalEnv,nil)), env);
}

object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}

object *fn_makunbound (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(MAKUNBOUND, notasymbol, var);
  delassoc(var, &GlobalEnv);
  return var;
}

object *fn_break (object *args, object *env) {
  (void) args;
  pfstring(PSTR("\nBreak!\n"), pserial);
  BreakLevel++;
  repl(env);
  BreakLevel--;
  return nil;
}

object *fn_read (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return read(gfun);
}

object *fn_prin1 (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  printobject(obj, pfun);
  return obj;
}

object *fn_print (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  printobject(obj, pfun);
  pfun(' ');
  return obj;
}

object *fn_princ (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  prin1object(obj, pfun);
  return obj;
}

object *fn_terpri (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  pln(pfun);
  return nil;
}

object *fn_readbyte (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  int c = gfun();
  return (c == -1) ? nil : number(c);
}

object *fn_readline (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return readstring('\n', gfun, false);
}

object *fn_writebyte (object *args, object *env) {
  (void) env;
  int value = checkinteger(WRITEBYTE, first(args));
  pfun_t pfun = pstreamfun(cdr(args));
  (pfun)(value);
  return nil;
}

object *fn_writestring (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  Flags = temp;
  return nil;
}

object *fn_writeline (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  pln(pfun);
  Flags = temp;
  return nil;
}

object *fn_restarti2c (object *args, object *env) {
  (void) env;
  int stream = first(args)->integer;
  args = cdr(args);
  int read = 0; // Write
  I2Ccount = 0;
  if (args != NULL) {
    object *rw = first(args);
    if (integerp(rw)) I2Ccount = rw->integer;
    read = (rw != NULL);
  }
  int address = stream & 0xFF;
  if (stream>>8 != I2CSTREAM) error2(RESTARTI2C, PSTR("not an i2c stream"));
  TwoWire *port;
  if (address < 128) port = &Wire;
  #if defined(ULISP_I2C1)
  else port = &Wire1;
  #endif
  return I2Crestart(port, address & 0x7F, read) ? tee : nil;
}

object *fn_gc (object *obj, object *env) {
  int initial = Freespace;
  unsigned long start = micros();
  gc(obj, env );
  unsigned long elapsed = micros() - start;
  pfstring(PSTR("Space: "), pserial);
  pint(Freespace - initial, pserial);
  pfstring(PSTR(" bytes, Time: "), pserial);
  pint(elapsed, pserial);
  pfstring(PSTR(" us\n"), pserial);
  return nil;
}

object *fn_room (object *args, object *env) {
  (void) args, (void) env;
  return number(Freespace);
}

object *fn_saveimage (object *args, object *env) {
  if (args != NULL) args = eval(first(args), env);
  return number(saveimage(args));
}

object *fn_loadimage (object *args, object *env) {
  (void) env;
  if (args != NULL) args = first(args);
  return number(loadimage(args));
}

object *fn_cls (object *args, object *env) {
  (void) args, (void) env;
  pserial(12);
  return nil;
}

// Arduino procedures

object *fn_pinmode (object *args, object *env) {
  (void) env; int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(NIL, arg);
  else pin = checkinteger(PINMODE, first(args));
  int pm = INPUT;
  arg = second(args);
  if (keywordp(arg)) pm = checkkeyword(PINMODE, arg);
  else if (integerp(arg)) {
    int mode = arg->integer;
    if (mode == 1) pm = OUTPUT; else if (mode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (mode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (arg != nil) pm = OUTPUT;
  pinMode(pin, pm);
  return nil;
}

object *fn_digitalread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALREAD, first(args));
  if (digitalRead(pin) != 0) return tee; else return nil;
}

object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(NIL, arg);
  else pin = checkinteger(DIGITALWRITE, arg);
  arg = second(args);
  int mode;
  if (keywordp(arg)) mode = checkkeyword(DIGITALWRITE, arg);
  else if (integerp(arg)) mode = arg->integer ? HIGH : LOW;
  else mode = (arg != nil) ? HIGH : LOW;
  digitalWrite(pin, mode);
  return arg;
}

object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(ANALOGREAD, arg);
  else {
    pin = checkinteger(ANALOGREAD, arg);
    checkanalogread(pin);
  }
  return number(analogRead(pin));
}

object *fn_analogreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620) || defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_RASPBERRY_PI_PICO_W) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040) || defined(ARDUINO_APOLLO3_SFE_ARTEMIS_ATP)
  error2(ANALOGREFERENCE, PSTR("not supported"));
  #else
  analogReference((eAnalogReference)checkkeyword(ANALOGREFERENCE, arg));
  #endif
  return arg;
}

object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ARDUINO_RASPBERRY_PI_PICO) || defined(ARDUINO_ADAFRUIT_FEATHER_RP2040) || defined(ARDUINO_ADAFRUIT_QTPY_RP2040)
  error2(ANALOGREADRESOLUTION, PSTR("not supported"));
  #else
  analogReadResolution(checkinteger(ANALOGREADRESOLUTION, arg));
  #endif
  return arg;
}

object *fn_analogwrite (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(NIL, arg);
  else pin = checkinteger(ANALOGWRITE, arg);
  checkanalogwrite(pin);
  object *value = second(args);
  analogWrite(pin, checkinteger(ANALOGWRITE, value));
  return value;
}

object *fn_analogwriteresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogWriteResolution(checkinteger(ANALOGWRITERESOLUTION, arg));
  return arg;
}

object *fn_delay (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  delay(checkinteger(DELAY, arg1));
  return arg1;
}

object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}

object *fn_sleep (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  sleep(checkinteger(SLEEP, arg1));
  return arg1;
}

object *fn_note (object *args, object *env) {
  (void) env;
  static int pin = 255;
  if (args != NULL) {
    pin = checkinteger(NOTE, first(args));
    int note = 0;
    if (cddr(args) != NULL) note = checkinteger(NOTE, second(args));
    int octave = 0;
    if (cddr(args) != NULL) octave = checkinteger(NOTE, third(args));
    playnote(pin, note, octave);
  } else nonote(pin);
  return nil;
}

object *fn_register (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int addr;
  if (keywordp(arg)) addr = checkkeyword(REGISTER, arg);
  else addr = checkinteger(REGISTER, first(args));
  if (cdr(args) == NULL) return number(*(uint32_t *)addr);
  (*(uint32_t *)addr) = checkinteger(REGISTER, second(args));
  return second(args);
}

// Tree Editor

object *fn_edit (object *args, object *env) {
  object *fun = first(args);
  object *pair = findvalue(EDIT, fun, env);
  clrflag(EXITEDITOR);
  object *arg = edit(eval(fun, env));
  cdr(pair) = arg;
  return arg;
}

// Pretty printer

object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  #if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
  #endif
  pln(pfun);
  superprint(obj, 0, pfun);
  ppwidth = PPWIDTH;
  return bsymbol(NOTHING);
}

object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  #if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
  #endif
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && builtin(car(val)->name) == LAMBDA) {
      superprint(cons(bsymbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(bsymbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(bsymbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pfun);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  ppwidth = PPWIDTH;
  return bsymbol(NOTHING);
}

// Format

object *fn_format (object *args, object *env) {
  (void) env;
  pfun_t pfun = pserial;
  object *output = first(args);
  object *obj;
  if (output == nil) { obj = startstring(FORMAT); pfun = pstr; }
  else if (output != tee) pfun = pstreamfun(args);
  object *formatstr = checkstring(FORMAT, second(args));
  object *save = NULL;
  args = cddr(args);
  int len = stringlength(formatstr);
  uint8_t n = 0, width = 0, w, bra = 0;
  char pad = ' ';
  bool tilde = false, mute = false, comma, quote;
  while (n < len) {
    char ch = nthchar(formatstr, n);
    char ch2 = ch & ~0x20; // force to upper case
    if (tilde) {
     if (ch == '}') {
        if (save == NULL) formaterr(formatstr, PSTR("no matching ~{"), n);
        if (args == NULL) { args = cdr(save); save = NULL; } else n = bra;
        mute = false; tilde = false;
      }
      else if (!mute) {
        if (comma && quote) { pad = ch; comma = false, quote = false; }
        else if (ch == '\'') {
          if (comma) quote = true;
          else formaterr(formatstr, PSTR("quote not valid"), n);
        }
        else if (ch == '~') { pfun('~'); tilde = false; }
        else if (ch >= '0' && ch <= '9') width = width*10 + ch - '0';
        else if (ch == ',') comma = true;
        else if (ch == '%') { pln(pfun); tilde = false; }
        else if (ch == '&') { pfl(pfun); tilde = false; }
        else if (ch == '^') {
          if (save != NULL && args == NULL) mute = true;
          tilde = false;
        }
        else if (ch == '{') {
          if (save != NULL) formaterr(formatstr, PSTR("can't nest ~{"), n);
          if (args == NULL) formaterr(formatstr, noargument, n);
          if (!listp(first(args))) formaterr(formatstr, notalist, n);
          save = args; args = first(args); bra = n; tilde = false;
          if (args == NULL) mute = true;
        }
        else if (ch2 == 'A' || ch2 == 'S' || ch2 == 'D' || ch2 == 'G' || ch2 == 'X' || ch2 == 'B') {
          if (args == NULL) formaterr(formatstr, noargument, n);
          object *arg = first(args); args = cdr(args);
          uint8_t aw = atomwidth(arg);
          if (width < aw) w = 0; else w = width-aw;
          tilde = false;
          if (ch2 == 'A') { prin1object(arg, pfun); indent(w, pad, pfun); }
          else if (ch2 == 'S') { printobject(arg, pfun); indent(w, pad, pfun); }
          else if (ch2 == 'D' || ch2 == 'G') { indent(w, pad, pfun); prin1object(arg, pfun); }
          else if (ch2 == 'X' || ch2 == 'B') {
            if (integerp(arg)) {
              uint8_t base = (ch2 == 'B') ? 2 : 16;
              uint8_t hw = basewidth(arg, base); if (width < hw) w = 0; else w = width-hw;
              indent(w, pad, pfun); pintbase(arg->integer, base, pfun);
            } else {
              indent(w, pad, pfun); prin1object(arg, pfun);
            }
          }
          tilde = false;
        } else formaterr(formatstr, PSTR("invalid directive"), n);
      }
    } else {
      if (ch == '~') { tilde = true; pad = ' '; width = 0; comma = false; quote = false; }
      else if (!mute) pfun(ch);
    }
    n++;
  }
  if (output == nil) return obj;
  else return nil;
}

// LispLibrary

object *fn_require (object *args, object *env) {
  object *arg = first(args);
  object *globals = GlobalEnv;
  if (!symbolp(arg)) error(REQUIRE, notasymbol, arg);
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    if (symbolp(var) && var == arg) return nil;
    globals = cdr(globals);
  }
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    // Is this the definition we want
    symbol_t fname = first(line)->name;
    if ((fname == sym(DEFUN) || fname == sym(DEFVAR)) && symbolp(second(line)) && second(line)->name == arg->name) {
      eval(line, env);
      return tee;
    }
    line = read(glibrary);
  }
  return nil;
}

object *fn_listlibrary (object *args, object *env) {
  (void) args, (void) env;
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    builtin_t bname = builtin(first(line)->name);
    if (bname == DEFUN || bname == DEFVAR) {
      printsymbol(second(line), pserial); pserial(' ');
    }
    line = read(glibrary);
  }
  return bsymbol(NOTHING);
}

// Documentation

object *fn_documentation (object *args, object *env) {
  return documentation(DOCUMENTATION, first(args), env);
}

// Wi-fi

object *fn_available (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(AVAILABLE, PSTR("invalid stream"));
  return number(client.available());
  #else
  (void) args, (void) env;
  error2(AVAILABLE, PSTR("not supported"));
  return nil;
  #endif
}

object *fn_wifiserver (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) args, (void) env;
  server.begin();
  return nil;
  #else
  (void) args, (void) env;
  error2(WIFISERVER, PSTR("not supported"));
  return nil;
  #endif
}

object *fn_wifisoftap (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  char ssid[33], pass[65];
  object *first = first(args); args = cdr(args);
  if (args == NULL) WiFi.beginAP(cstring(WIFISOFTAP, first, ssid, 33));
  else {
    object *second = first(args);
    args = cdr(args);
    int channel = 1;
    if (args != NULL) {
      channel = checkinteger(WIFISOFTAP, first(args));
      args = cdr(args);
    }
    WiFi.beginAP(cstring(WIFISOFTAP, first, ssid, 33), cstring(WIFISOFTAP, second, pass, 65), channel);
  }
  return lispstring((char*)"192.168.4.1");
  #else
  (void) args, (void) env;
  error2(WIFISOFTAP, PSTR("not supported"));
  return nil;
  #endif
}

object *fn_connected (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(CONNECTED, PSTR("invalid stream"));
  return client.connected() ? tee : nil;
  #else
  (void) args, (void) env;
  error2(CONNECTED, PSTR("not supported"));
  return nil;
  #endif
}

object *fn_wifilocalip (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) args, (void) env;
  return lispstring((char*)WiFi.localIP().toString().c_str());
  #else
  (void) args, (void) env;
  error2(WIFILOCALIP, PSTR("not supported"));
  return nil;
  #endif
}

object *fn_wificonnect (object *args, object *env) {
  #if defined (ULISP_WIFI)
  (void) env;
  char ssid[33], pass[65];
  if (args == NULL) { WiFi.disconnect(); return nil; }
  if (cdr(args) == NULL) WiFi.begin(cstring(WIFICONNECT, first(args), ssid, 33));
  else {
    if (cddr(args) != NULL) WiFi.config(ipstring(WIFICONNECT, third(args)));
    WiFi.begin(cstring(WIFICONNECT, first(args), ssid, 33), cstring(WIFICONNECT, second(args), pass, 65));
  }
  int result = WiFi.waitForConnectResult();
  if (result == WL_CONNECTED) return lispstring((char*)WiFi.localIP().toString().c_str());
  else if (result == WL_NO_SSID_AVAIL) error2(WIFICONNECT, PSTR("network not found"));
  else if (result == WL_CONNECT_FAILED) error2(WIFICONNECT, PSTR("connection failed"));
  else error2(WIFICONNECT, PSTR("unable to connect"));
  return nil;
  #else
  (void) args, (void) env;
  error2(WIFICONNECT, PSTR("not supported"));
  return nil;
  #endif
}

// Graphics functions

object *fn_drawpixel (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(DRAWPIXEL, third(args));
  tft.drawPixel(checkinteger(DRAWPIXEL, first(args)), checkinteger(DRAWPIXEL, second(args)), colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawline (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWLINE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWLINE, car(args));
  tft.drawLine(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWRECT, car(args));
  tft.drawRect(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_fillrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(FILLRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLRECT, car(args));
  tft.fillRect(params[0], params[1], params[2], params[3], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawcircle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(DRAWCIRCLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWCIRCLE, car(args));
  tft.drawCircle(params[0], params[1], params[2], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_fillcircle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(FILLCIRCLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLCIRCLE, car(args));
  tft.fillCircle(params[0], params[1], params[2], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawroundrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(DRAWROUNDRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWROUNDRECT, car(args));
  tft.drawRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_fillroundrect (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(FILLROUNDRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLROUNDRECT, car(args));
  tft.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawtriangle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(DRAWTRIANGLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWTRIANGLE, car(args));
  tft.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_filltriangle (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(FILLTRIANGLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLTRIANGLE, car(args));
  tft.fillTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_drawchar (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_WHITE, bg = COLOR_BLACK, size = 1;
  object *more = cdr(cddr(args));
  if (more != NULL) {
    colour = checkinteger(DRAWCHAR, car(more));
    more = cdr(more);
    if (more != NULL) {
      bg = checkinteger(DRAWCHAR, car(more));
      more = cdr(more);
      if (more != NULL) size = checkinteger(DRAWCHAR, car(more));
    }
  }
  tft.drawChar(checkinteger(DRAWCHAR, first(args)), checkinteger(DRAWCHAR, second(args)), checkchar(DRAWCHAR, third(args)),
    colour, bg, size);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_setcursor (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setCursor(checkinteger(SETCURSOR, first(args)), checkinteger(SETCURSOR, second(args)));
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_settextcolor (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  if (cdr(args) != NULL) tft.setTextColor(checkinteger(SETTEXTCOLOR, first(args)), checkinteger(SETTEXTCOLOR, second(args)));
  else tft.setTextColor(checkinteger(SETTEXTCOLOR, first(args)));
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_settextsize (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  int size = checkinteger(SETTEXTSIZE, first(args));
  
  if(size == 0) {
    tft.setFont(&Picopixel);
  } else {
    tft.setFont();
  }

  tft.setTextSize(size);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_settextwrap (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setTextWrap(first(args) != NULL);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_fillscreen (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(FILLSCREEN, first(args));
  tft.fillScreen(colour);
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_setrotation (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.setRotation(checkinteger(SETROTATION, first(args)));
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_invertdisplay (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  tft.invertDisplay(first(args) != NULL);
  #else
  (void) args;
  #endif
  return nil;
}


// Insert your own function definitions here

object *fn_refresh (object *args, object *env) {
  (void) env;
  #if defined(gfxsupport)
  disableISR();  
  tft.refresh();
  enableISR();
  #else
  (void) args;
  #endif
  return nil;
}

object *fn_getkey (object *args, object *env) {
  (void) env;
  (void) args;
  uint32_t readTo = keyEventWritePtr;
  if(keyEventReadPtr < readTo) {
    int k = keyEvents[keyEventReadPtr % KEYEVENT_BUFFER_SIZE];
    keyEventReadPtr++;
    return number(k);
  }

  // andreer: Reduces power consumption when a program is waiting for input in a loop
  am_hal_sysctrl_sleep(AM_HAL_SYSCTRL_DEEPSLEEP);
  return nil;
}

object *fn_peek (object *args, object *env) {
  (void) env;
  int addr = checkinteger(PEEK, first(args));
  return number(*(int *)addr);

  // long t0 = millis();
  // tft.setCursor(0, 0);
  // for(int i = 0; i < 1600; i++) {
  //   tft.write('0' + (random()%64));
  // }
  // long t1 = millis();
  // Serial.print("Filling screen with chars took ");
  // Serial.print(t1-t0);
  // Serial.println(" ms");

  // return number(42);
}

object *fn_poke (object *args, object *env) {
  (void) env;
  int addr = checkinteger(POKE, first(args));
  object *val = second(args);
  *(int *)addr = checkinteger(POKE, val);
  return val;
}

object *fn_subseql (object *args, object *env) {
  // not an elegant implementation, but much faster than an interpreted one
  (void) env;
  object *list = first(args);
  if (!listp(list)) error(SUBSEQL, notalist, list);
  int start = checkinteger(SUBSEQL, second(args));
  int end = -1;
  if (cddr(args) != NULL) {
    end = checkinteger(SUBSEQL, third(args));
  } else {
    end = 1<<30;
  }

  if(start < 0 || end <= start) return NULL;

  object *res = NULL;
  int skip = start;
  int take = end-start;

  for(int i = 0; i < skip; i++) {
    if (list == NULL) return NULL;
    if (improperp(list)) error(SUBSEQL, notproper, list);
    list = cdr(list);
  }

  for (int i = 0; i < take; i++) {
    if (list == NULL) break;
    if (improperp(list)) error(SUBSEQL, notproper, list);
    res = cons(car(list), res);
    list = cdr(list);
  }

  return res; // This is reversed, but there's another built-in to reverse so I didn't bother
}

// Built-in symbol names
const char string0[] PROGMEM = "nil";
const char string1[] PROGMEM = "t";
const char string2[] PROGMEM = "nothing";
const char string3[] PROGMEM = "&optional";
const char string4[] PROGMEM = ":initial-element";
const char string5[] PROGMEM = ":element-type";
const char string6[] PROGMEM = "bit";
const char string7[] PROGMEM = "&rest";
const char string8[] PROGMEM = "lambda";
const char string9[] PROGMEM = "let";
const char string10[] PROGMEM = "let*";
const char string11[] PROGMEM = "closure";
const char string12[] PROGMEM = "*pc*";
const char string13[] PROGMEM = "";
const char string14[] PROGMEM = "quote";
const char string15[] PROGMEM = "or";
const char string16[] PROGMEM = "defun";
const char string17[] PROGMEM = "defvar";
const char string18[] PROGMEM = "setq";
const char string19[] PROGMEM = "loop";
const char string20[] PROGMEM = "return";
const char string21[] PROGMEM = "push";
const char string22[] PROGMEM = "pop";
const char string23[] PROGMEM = "incf";
const char string24[] PROGMEM = "decf";
const char string25[] PROGMEM = "setf";
const char string26[] PROGMEM = "dolist";
const char string27[] PROGMEM = "dotimes";
const char string28[] PROGMEM = "trace";
const char string29[] PROGMEM = "untrace";
const char string30[] PROGMEM = "for-millis";
const char string31[] PROGMEM = "time";
const char string32[] PROGMEM = "with-output-to-string";
const char string33[] PROGMEM = "with-serial";
const char string34[] PROGMEM = "with-i2c";
const char string35[] PROGMEM = "with-spi";
const char string36[] PROGMEM = "with-sd-card";
const char string37[] PROGMEM = "with-gfx";
const char string38[] PROGMEM = "with-client";
const char string944d81fd0ed3aa6433ad9b560576[] PROGMEM = "unwind-protect";
const char string4c085b45ade192ea2a98b36e8a24[] PROGMEM = "ignore-errors";
const char stringda16dce31347cc2b47fc4bac32ed[] PROGMEM = "error";
const char string39[] PROGMEM = "defcode";
const char string40[] PROGMEM = "";
const char string41[] PROGMEM = "progn";
const char string42[] PROGMEM = "if";
const char string43[] PROGMEM = "cond";
const char string44[] PROGMEM = "when";
const char string45[] PROGMEM = "unless";
const char string46[] PROGMEM = "case";
const char string47[] PROGMEM = "and";
const char string48[] PROGMEM = "?";
const char string49[] PROGMEM = "";
const char string50[] PROGMEM = "not";
const char string51[] PROGMEM = "null";
const char string52[] PROGMEM = "cons";
const char string53[] PROGMEM = "atom";
const char string54[] PROGMEM = "listp";
const char string55[] PROGMEM = "consp";
const char string56[] PROGMEM = "symbolp";
const char string57[] PROGMEM = "arrayp";
const char string58[] PROGMEM = "boundp";
const char string59[] PROGMEM = "set";
const char string60[] PROGMEM = "streamp";
const char string61[] PROGMEM = "eq";
const char string62[] PROGMEM = "car";
const char string63[] PROGMEM = "first";
const char string64[] PROGMEM = "cdr";
const char string65[] PROGMEM = "rest";
const char string66[] PROGMEM = "caar";
const char string67[] PROGMEM = "cadr";
const char string68[] PROGMEM = "second";
const char string69[] PROGMEM = "cdar";
const char string70[] PROGMEM = "cddr";
const char string71[] PROGMEM = "caaar";
const char string72[] PROGMEM = "caadr";
const char string73[] PROGMEM = "cadar";
const char string74[] PROGMEM = "caddr";
const char string75[] PROGMEM = "third";
const char string76[] PROGMEM = "cdaar";
const char string77[] PROGMEM = "cdadr";
const char string78[] PROGMEM = "cddar";
const char string79[] PROGMEM = "cdddr";
const char string80[] PROGMEM = "length";
const char string81[] PROGMEM = "array-dimensions";
const char string82[] PROGMEM = "list";
const char string83[] PROGMEM = "make-array";
const char string84[] PROGMEM = "reverse";
const char string85[] PROGMEM = "nth";
const char string86[] PROGMEM = "aref";
const char string87[] PROGMEM = "assoc";
const char string88[] PROGMEM = "member";
const char string89[] PROGMEM = "apply";
const char string90[] PROGMEM = "funcall";
const char string91[] PROGMEM = "append";
const char string92[] PROGMEM = "mapc";
const char string93[] PROGMEM = "mapcar";
const char string94[] PROGMEM = "mapcan";
const char string95[] PROGMEM = "+";
const char string96[] PROGMEM = "-";
const char string97[] PROGMEM = "*";
const char string98[] PROGMEM = "/";
const char string99[] PROGMEM = "mod";
const char string100[] PROGMEM = "1+";
const char string101[] PROGMEM = "1-";
const char string102[] PROGMEM = "abs";
const char string103[] PROGMEM = "random";
const char string104[] PROGMEM = "max";
const char string105[] PROGMEM = "min";
const char string106[] PROGMEM = "/=";
const char string107[] PROGMEM = "=";
const char string108[] PROGMEM = "<";
const char string109[] PROGMEM = "<=";
const char string110[] PROGMEM = ">";
const char string111[] PROGMEM = ">=";
const char string112[] PROGMEM = "plusp";
const char string113[] PROGMEM = "minusp";
const char string114[] PROGMEM = "zerop";
const char string115[] PROGMEM = "oddp";
const char string116[] PROGMEM = "evenp";
const char string117[] PROGMEM = "integerp";
const char string118[] PROGMEM = "numberp";
const char string119[] PROGMEM = "float";
const char string120[] PROGMEM = "floatp";
const char string121[] PROGMEM = "sin";
const char string122[] PROGMEM = "cos";
const char string123[] PROGMEM = "tan";
const char string124[] PROGMEM = "asin";
const char string125[] PROGMEM = "acos";
const char string126[] PROGMEM = "atan";
const char string127[] PROGMEM = "sinh";
const char string128[] PROGMEM = "cosh";
const char string129[] PROGMEM = "tanh";
const char string130[] PROGMEM = "exp";
const char string131[] PROGMEM = "sqrt";
const char string132[] PROGMEM = "log";
const char string133[] PROGMEM = "expt";
const char string134[] PROGMEM = "ceiling";
const char string135[] PROGMEM = "floor";
const char string136[] PROGMEM = "truncate";
const char string137[] PROGMEM = "round";
const char string138[] PROGMEM = "char";
const char string139[] PROGMEM = "char-code";
const char string140[] PROGMEM = "code-char";
const char string141[] PROGMEM = "characterp";
const char string142[] PROGMEM = "stringp";
const char string143[] PROGMEM = "string=";
const char string144[] PROGMEM = "string<";
const char string145[] PROGMEM = "string>";
const char string146[] PROGMEM = "sort";
const char string147[] PROGMEM = "string";
const char string148[] PROGMEM = "concatenate";
const char string149[] PROGMEM = "subseq";
const char string150[] PROGMEM = "read-from-string";
const char string151[] PROGMEM = "princ-to-string";
const char string152[] PROGMEM = "prin1-to-string";
const char string153[] PROGMEM = "logand";
const char string154[] PROGMEM = "logior";
const char string155[] PROGMEM = "logxor";
const char string156[] PROGMEM = "lognot";
const char string157[] PROGMEM = "ash";
const char string158[] PROGMEM = "logbitp";
const char string159[] PROGMEM = "eval";
const char string160[] PROGMEM = "globals";
const char string161[] PROGMEM = "locals";
const char string162[] PROGMEM = "makunbound";
const char string163[] PROGMEM = "break";
const char string164[] PROGMEM = "read";
const char string165[] PROGMEM = "prin1";
const char string166[] PROGMEM = "print";
const char string167[] PROGMEM = "princ";
const char string168[] PROGMEM = "terpri";
const char string169[] PROGMEM = "read-byte";
const char string170[] PROGMEM = "read-line";
const char string171[] PROGMEM = "write-byte";
const char string172[] PROGMEM = "write-string";
const char string173[] PROGMEM = "write-line";
const char string174[] PROGMEM = "restart-i2c";
const char string175[] PROGMEM = "gc";
const char string176[] PROGMEM = "room";
const char string177[] PROGMEM = "save-image";
const char string178[] PROGMEM = "load-image";
const char string179[] PROGMEM = "cls";
const char string180[] PROGMEM = "pinmode";
const char string181[] PROGMEM = "digitalread";
const char string182[] PROGMEM = "digitalwrite";
const char string183[] PROGMEM = "analogread";
const char string184[] PROGMEM = "analogreference";
const char string185[] PROGMEM = "analogreadresolution";
const char string186[] PROGMEM = "analogwrite";
const char string187[] PROGMEM = "analogwriteresolution";
const char string188[] PROGMEM = "delay";
const char string189[] PROGMEM = "millis";
const char string190[] PROGMEM = "sleep";
const char string191[] PROGMEM = "note";
const char string192[] PROGMEM = "register";
const char string193[] PROGMEM = "edit";
const char string194[] PROGMEM = "pprint";
const char string195[] PROGMEM = "pprintall";
const char string196[] PROGMEM = "format";
const char string197[] PROGMEM = "require";
const char string198[] PROGMEM = "list-library";
const char string199[] PROGMEM = "documentation";
const char string200[] PROGMEM = "available";
const char string201[] PROGMEM = "wifi-server";
const char string202[] PROGMEM = "wifi-softap";
const char string203[] PROGMEM = "connected";
const char string204[] PROGMEM = "wifi-localip";
const char string205[] PROGMEM = "wifi-connect";
const char string206[] PROGMEM = "draw-pixel";
const char string207[] PROGMEM = "draw-line";
const char string208[] PROGMEM = "draw-rect";
const char string209[] PROGMEM = "fill-rect";
const char string210[] PROGMEM = "draw-circle";
const char string211[] PROGMEM = "fill-circle";
const char string212[] PROGMEM = "draw-round-rect";
const char string213[] PROGMEM = "fill-round-rect";
const char string214[] PROGMEM = "draw-triangle";
const char string215[] PROGMEM = "fill-triangle";
const char string216[] PROGMEM = "draw-char";
const char string217[] PROGMEM = "set-cursor";
const char string218[] PROGMEM = "set-text-color";
const char string219[] PROGMEM = "set-text-size";
const char string220[] PROGMEM = "set-text-wrap";
const char string221[] PROGMEM = "fill-screen";
const char string222[] PROGMEM = "set-rotation";
const char string223[] PROGMEM = "invert-display";
const char string224[] PROGMEM = "";
const char string225[] PROGMEM = ":led-builtin";
const char string226[] PROGMEM = ":high";
const char string227[] PROGMEM = ":low";
#if defined(CPU_APOLLO3)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = "";
#elif defined(CPU_ATSAMD21)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":ar-default";
const char string233[] PROGMEM = ":ar-internal1v0";
const char string234[] PROGMEM = ":ar-internal1v65";
const char string235[] PROGMEM = ":ar-internal2v23";
const char string236[] PROGMEM = ":ar-external";
const char string237[] PROGMEM = ":pa-dir";
const char string238[] PROGMEM = ":pa-dirclr";
const char string239[] PROGMEM = ":pa-dirset";
const char string240[] PROGMEM = ":pa-dirtgl";
const char string241[] PROGMEM = ":pa-out";
const char string242[] PROGMEM = ":pa-outclr";
const char string243[] PROGMEM = ":pa-outset";
const char string244[] PROGMEM = ":pa-outtgl";
const char string245[] PROGMEM = ":pa-in";
const char string246[] PROGMEM = ":pb-dir";
const char string247[] PROGMEM = ":pb-dirclr";
const char string248[] PROGMEM = ":pb-dirset";
const char string249[] PROGMEM = ":pb-dirtgl";
const char string250[] PROGMEM = ":pb-out";
const char string251[] PROGMEM = ":pb-outclr";
const char string252[] PROGMEM = ":pb-outset";
const char string253[] PROGMEM = ":pb-outtgl";
const char string254[] PROGMEM = ":pb-in";
const char string255[] PROGMEM = "";
#elif defined(CPU_ATSAMD51)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":ar-default";
const char string233[] PROGMEM = ":ar-internal1v0";
const char string234[] PROGMEM = ":ar-internal1v1";
const char string235[] PROGMEM = ":ar-internal1v2";
const char string236[] PROGMEM = ":ar-internal1v25";
const char string237[] PROGMEM = ":ar-internal1v65";
const char string238[] PROGMEM = ":ar-internal2v0";
const char string239[] PROGMEM = ":ar-internal2v2";
const char string240[] PROGMEM = ":ar-internal2v23";
const char string241[] PROGMEM = ":ar-internal2v4";
const char string242[] PROGMEM = ":ar-internal2v5";
const char string243[] PROGMEM = ":ar-external";
const char string244[] PROGMEM = ":pa-dir";
const char string245[] PROGMEM = ":pa-dirclr";
const char string246[] PROGMEM = ":pa-dirset";
const char string247[] PROGMEM = ":pa-dirtgl";
const char string248[] PROGMEM = ":pa-out";
const char string249[] PROGMEM = ":pa-outclr";
const char string250[] PROGMEM = ":pa-outset";
const char string251[] PROGMEM = ":pa-outtgl";
const char string252[] PROGMEM = ":pa-in";
const char string253[] PROGMEM = ":pb-dir";
const char string254[] PROGMEM = ":pb-dirclr";
const char string255[] PROGMEM = ":pb-dirset";
const char string256[] PROGMEM = ":pb-dirtgl";
const char string257[] PROGMEM = ":pb-out";
const char string258[] PROGMEM = ":pb-outclr";
const char string259[] PROGMEM = ":pb-outset";
const char string260[] PROGMEM = ":pb-outtgl";
const char string261[] PROGMEM = ":pb-in";
const char string262[] PROGMEM = "";
#elif defined(CPU_NRF51822)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":ar-default";
const char string233[] PROGMEM = ":ar-vbg";
const char string234[] PROGMEM = ":ar-supply-one-half";
const char string235[] PROGMEM = ":ar-supply-one-third";
const char string236[] PROGMEM = ":ar-ext0";
const char string237[] PROGMEM = ":ar-ext1";
const char string238[] PROGMEM = ":p0-out";
const char string239[] PROGMEM = ":p0-outset";
const char string240[] PROGMEM = ":p0-outclr";
const char string241[] PROGMEM = ":p0-in";
const char string242[] PROGMEM = ":p0-dir";
const char string243[] PROGMEM = ":p0-dirset";
const char string244[] PROGMEM = ":p0-dirclr";
const char string245[] PROGMEM = "";
#elif defined(CPU_NRF52840)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":ar-default";
const char string233[] PROGMEM = ":ar-internal";
const char string234[] PROGMEM = ":ar-internal-3-0";
const char string235[] PROGMEM = ":ar-internal-2-4";
const char string236[] PROGMEM = ":ar-internal-1-8";
const char string237[] PROGMEM = ":ar-internal-1-2";
const char string238[] PROGMEM = ":ar-vdd4";
const char string239[] PROGMEM = ":p0-out";
const char string240[] PROGMEM = ":p0-outset";
const char string241[] PROGMEM = ":p0-outclr";
const char string242[] PROGMEM = ":p0-in";
const char string243[] PROGMEM = ":p0-dir";
const char string244[] PROGMEM = ":p0-dirset";
const char string245[] PROGMEM = ":p0-dirclr";
const char string246[] PROGMEM = ":p1-out";
const char string247[] PROGMEM = ":p1-outset";
const char string248[] PROGMEM = ":p1-outclr";
const char string249[] PROGMEM = ":p1-in";
const char string250[] PROGMEM = ":p1-dir";
const char string251[] PROGMEM = ":p1-dirset";
const char string252[] PROGMEM = ":p1-dirclr";
const char string253[] PROGMEM = "";
#elif defined(CPU_NRF52833)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":ar-default";
const char string233[] PROGMEM = ":ar-internal";
const char string234[] PROGMEM = ":ar-vdd4";
const char string235[] PROGMEM = ":p0-out";
const char string236[] PROGMEM = ":p0-outset";
const char string237[] PROGMEM = ":p0-outclr";
const char string238[] PROGMEM = ":p0-in";
const char string239[] PROGMEM = ":p0-dir";
const char string240[] PROGMEM = ":p0-dirset";
const char string241[] PROGMEM = ":p0-dirclr";
const char string242[] PROGMEM = ":p1-out";
const char string243[] PROGMEM = ":p1-outset";
const char string244[] PROGMEM = ":p1-outclr";
const char string245[] PROGMEM = ":p1-in";
const char string246[] PROGMEM = ":p1-dir";
const char string247[] PROGMEM = ":p1-dirset";
const char string248[] PROGMEM = ":p1-dirclr";
const char string249[] PROGMEM = "";
#elif defined(CPU_iMXRT1062)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":output-opendrain";
const char string233[] PROGMEM = "";
#elif defined(CPU_MAX32620)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":output";
const char string231[] PROGMEM = ":default";
const char string232[] PROGMEM = ":external";
const char string233[] PROGMEM = "";
#elif defined(CPU_RP2040)
const char string228[] PROGMEM = ":input";
const char string229[] PROGMEM = ":input-pullup";
const char string230[] PROGMEM = ":input-pulldown";
const char string231[] PROGMEM = ":output";
const char string232[] PROGMEM = ":gpio-in";
const char string233[] PROGMEM = ":gpio-out";
const char string234[] PROGMEM = ":gpio-out-set";
const char string235[] PROGMEM = ":gpio-out-clr";
const char string236[] PROGMEM = ":gpio-out-xor";
const char string237[] PROGMEM = ":gpio-oe";
const char string238[] PROGMEM = ":gpio-oe-set";
const char string239[] PROGMEM = ":gpio-oe-clr";
const char string240[] PROGMEM = ":gpio-oe-xor";
const char string241[] PROGMEM = "";
#endif

// Insert your own function names here

const char mystring1[] PROGMEM = "refresh";
const char mystring2[] PROGMEM = "get-key";
const char mystring3[] PROGMEM = "peek";
const char mystring4[] PROGMEM = "poke";
const char mystring5[] PROGMEM = "subseql";
const char user06975b647a442ae56f6ee0abc8fb[] PROGMEM = "get-error";

// Documentation strings
const char doc0[] PROGMEM = "nil\n"
"A symbol equivalent to the empty list (). Also represents false.";
const char doc1[] PROGMEM = "t\n"
"A symbol representing true.";
const char doc2[] PROGMEM = "nothing\n"
"A symbol with no value.\n"
"It is useful if you want to suppress printing the result of evaluating a function.";
const char doc3[] PROGMEM = "&optional\n"
"Can be followed by one or more optional parameters in a lambda or defun parameter list.";
const char doc7[] PROGMEM = "&rest\n"
"Can be followed by a parameter in a lambda or defun parameter list,\n"
"and is assigned a list of the corresponding arguments.";
const char doc8[] PROGMEM = "(lambda (parameter*) form*)\n"
"Creates an unnamed function with parameters. The body is evaluated with the parameters as local variables\n"
"whose initial values are defined by the values of the forms after the lambda form.";
const char doc9[] PROGMEM = "(let ((var value) ... ) forms*)\n"
"Declares local variables with values, and evaluates the forms with those local variables.";
const char doc10[] PROGMEM = "(let* ((var value) ... ) forms*)\n"
"Declares local variables with values, and evaluates the forms with those local variables.\n"
"Each declaration can refer to local variables that have been defined earlier in the let*.";
const char doc15[] PROGMEM = "(or item*)\n"
"Evaluates its arguments until one returns non-nil, and returns its value.";
const char doc16[] PROGMEM = "(defun name (parameters) form*)\n"
"Defines a function.";
const char doc17[] PROGMEM = "(defvar variable form)\n"
"Defines a global variable.";
const char doc18[] PROGMEM = "(setq symbol value [symbol value]*)\n"
"For each pair of arguments assigns the value of the second argument\n"
"to the variable specified in the first argument.";
const char doc19[] PROGMEM = "(loop forms*)\n"
"Executes its arguments repeatedly until one of the arguments calls (return),\n"
"which then causes an exit from the loop.";
const char doc20[] PROGMEM = "(return [value])\n"
"Exits from a (dotimes ...), (dolist ...), or (loop ...) loop construct and returns value.";
const char doc21[] PROGMEM = "(push item place)\n"
"Modifies the value of place, which should be a list, to add item onto the front of the list,\n"
"and returns the new list.";
const char doc22[] PROGMEM = "(pop place)\n"
"Modifies the value of place, which should be a list, to remove its first item, and returns that item.";
const char doc23[] PROGMEM = "(incf place [number])\n"
"Increments a place, which should have an numeric value, and returns the result.\n"
"The third argument is an optional increment which defaults to 1.";
const char doc24[] PROGMEM = "(decf place [number])\n"
"Decrements a place, which should have an numeric value, and returns the result.\n"
"The third argument is an optional decrement which defaults to 1.";
const char doc25[] PROGMEM = "(setf place value [place value]*)\n"
"For each pair of arguments modifies a place to the result of evaluating value.";
const char doc26[] PROGMEM = "(dolist (var list [result]) form*)\n"
"Sets the local variable var to each element of list in turn, and executes the forms.\n"
"It then returns result, or nil if result is omitted.";
const char doc27[] PROGMEM = "(dotimes (var number [result]) form*)\n"
"Executes the forms number times, with the local variable var set to each integer from 0 to number-1 in turn.\n"
"It then returns result, or nil if result is omitted.";
const char doc28[] PROGMEM = "(trace [function]*)\n"
"Turns on tracing of up to TRACEMAX user-defined functions,\n"
"and returns a list of the functions currently being traced.";
const char doc29[] PROGMEM = "(untrace [function]*)\n"
"Turns off tracing of up to TRACEMAX user-defined functions, and returns a list of the functions untraced.\n"
"If no functions are specified it untraces all functions.";
const char doc30[] PROGMEM = "(for-millis ([number]) form*)\n"
"Executes the forms and then waits until a total of number milliseconds have elapsed.\n"
"Returns the total number of milliseconds taken.";
const char doc31[] PROGMEM = "(time form)\n"
"Prints the value returned by the form, and the time taken to evaluate the form\n"
"in milliseconds or seconds.";
const char doc32[] PROGMEM = "(with-output-to-string (str) form*)\n"
"Returns a string containing the output to the stream variable str.";
const char doc33[] PROGMEM = "(with-serial (str port [baud]) form*)\n"
"Evaluates the forms with str bound to a serial-stream using port.\n"
"The optional baud gives the baud rate divided by 100, default 96.";
const char doc34[] PROGMEM = "(with-i2c (str [port] address [read-p]) form*)\n"
"Evaluates the forms with str bound to an i2c-stream defined by address.\n"
"If read-p is nil or omitted the stream is written to, otherwise it specifies the number of bytes\n"
"to be read from the stream. If port is omitted it defaults to 0, otherwise it specifies the port, 0 or 1.";
const char doc35[] PROGMEM = "(with-spi (str pin [clock] [bitorder] [mode] [port]) form*)\n"
"Evaluates the forms with str bound to an spi-stream.\n"
"The parameters specify the enable pin, clock in kHz (default 4000),\n"
"bitorder 0 for LSBFIRST and 1 for MSBFIRST (default 1), SPI mode (default 0), and port 0 or 1 (default 0).";
const char doc36[] PROGMEM = "(with-sd-card (str filename [mode]) form*)\n"
"Evaluates the forms with str bound to an sd-stream reading from or writing to the file filename.\n"
"If mode is omitted the file is read, otherwise 0 means read, 1 write-append, or 2 write-overwrite.";
const char doc37[] PROGMEM = "(with-gfx (str) form*)\n"
"Evaluates the forms with str bound to an gfx-stream so you can print text\n"
"to the graphics display using the standard uLisp print commands.";
const char doc38[] PROGMEM = "(with-client (str [address port]) form*)\n"
"Evaluates the forms with str bound to a wifi-stream.";
const char doc39[] PROGMEM = "(defcode name (parameters) form*)\n"
"Creates a machine-code function called name from a series of 16-bit integers given in the body of the form.\n"
"These are written into RAM, and can be executed by calling the function in the same way as a normal Lisp function.";
const char doc41[] PROGMEM = "(progn form*)\n"
"Evaluates several forms grouped together into a block, and returns the result of evaluating the last form.";
const char doc42[] PROGMEM = "(if test then [else])\n"
"Evaluates test. If it's non-nil the form then is evaluated and returned;\n"
"otherwise the form else is evaluated and returned.";
const char doc43[] PROGMEM = "(cond ((test form*) (test form*) ... ))\n"
"Each argument is a list consisting of a test optionally followed by one or more forms.\n"
"If the test evaluates to non-nil the forms are evaluated, and the last value is returned as the result of the cond.\n"
"If the test evaluates to nil, none of the forms are evaluated, and the next argument is processed in the same way.";
const char doc44[] PROGMEM = "(when test form*)\n"
"Evaluates the test. If it's non-nil the forms are evaluated and the last value is returned.";
const char doc45[] PROGMEM = "(unless test form*)\n"
"Evaluates the test. If it's nil the forms are evaluated and the last value is returned.";
const char doc46[] PROGMEM = "(case keyform ((key form*) (key form*) ... ))\n"
"Evaluates a keyform to produce a test key, and then tests this against a series of arguments,\n"
"each of which is a list containing a key optionally followed by one or more forms.";
const char doc47[] PROGMEM = "(and item*)\n"
"Evaluates its arguments until one returns nil, and returns the last value.";
const char doc48[] PROGMEM = "(? item)\n"
"Prints the documentation string of a built-in or user-defined function.";
const char doc50[] PROGMEM = "(not item)\n"
"Returns t if its argument is nil, or nil otherwise. Equivalent to null.";
const char doc52[] PROGMEM = "(cons item item)\n"
"If the second argument is a list, cons returns a new list with item added to the front of the list.\n"
"If the second argument isn't a list cons returns a dotted pair.";
const char doc53[] PROGMEM = "(atom item)\n"
"Returns t if its argument is a single number, symbol, or nil.";
const char doc54[] PROGMEM = "(listp item)\n"
"Returns t if its argument is a list.";
const char doc55[] PROGMEM = "(consp item)\n"
"Returns t if its argument is a non-null list.";
const char doc56[] PROGMEM = "(symbolp item)\n"
"Returns t if its argument is a symbol.";
const char doc57[] PROGMEM = "(arrayp item)\n"
"Returns t if its argument is an array.";
const char doc58[] PROGMEM = "(boundp item)\n"
"Returns t if its argument is a symbol with a value.";
const char doc59[] PROGMEM = "(set symbol value [symbol value]*)\n"
"For each pair of arguments, assigns the value of the second argument to the value of the first argument.";
const char doc60[] PROGMEM = "(streamp item)\n"
"Returns t if its argument is a stream.";
const char doc61[] PROGMEM = "(eq item item)\n"
"Tests whether the two arguments are the same symbol, same character, equal numbers,\n"
"or point to the same cons, and returns t or nil as appropriate.";
const char doc62[] PROGMEM = "(car list)\n"
"Returns the first item in a list.";
const char doc64[] PROGMEM = "(cdr list)\n"
"Returns a list with the first item removed.";
const char doc66[] PROGMEM = "(caar list)";
const char doc67[] PROGMEM = "(cadr list)";
const char doc69[] PROGMEM = "(cdar list)\n"
"Equivalent to (cdr (car list)).";
const char doc70[] PROGMEM = "(cddr list)\n"
"Equivalent to (cdr (cdr list)).";
const char doc71[] PROGMEM = "(caaar list)\n"
"Equivalent to (car (car (car list))).";
const char doc72[] PROGMEM = "(caadr list)\n"
"Equivalent to (car (car (cdar list))).";
const char doc73[] PROGMEM = "(cadar list)\n"
"Equivalent to (car (cdr (car list))).";
const char doc74[] PROGMEM = "(caddr list)\n"
"Equivalent to (car (cdr (cdr list))).";
const char doc76[] PROGMEM = "(cdaar list)\n"
"Equivalent to (cdar (car (car list))).";
const char doc77[] PROGMEM = "(cdadr list)\n"
"Equivalent to (cdr (car (cdr list))).";
const char doc78[] PROGMEM = "(cddar list)\n"
"Equivalent to (cdr (cdr (car list))).";
const char doc79[] PROGMEM = "(cdddr list)\n"
"Equivalent to (cdr (cdr (cdr list))).";
const char doc80[] PROGMEM = "(length item)\n"
"Returns the number of items in a list, the length of a string, or the length of a one-dimensional array.";
const char doc81[] PROGMEM = "(array-dimensions item)\n"
"Returns a list of the dimensions of an array.";
const char doc82[] PROGMEM = "(list item*)\n"
"Returns a list of the values of its arguments.";
const char doc83[] PROGMEM = "(make-array size [:initial-element element] [:element-type 'bit])\n"
"If size is an integer it creates a one-dimensional array with elements from 0 to size-1.\n"
"If size is a list of n integers it creates an n-dimensional array with those dimensions.\n"
"If :element-type 'bit is specified the array is a bit array.";
const char doc84[] PROGMEM = "(reverse list)\n"
"Returns a list with the elements of list in reverse order.";
const char doc85[] PROGMEM = "(nth number list)\n"
"Returns the nth item in list, counting from zero.";
const char doc86[] PROGMEM = "(aref array index [index*])\n"
"Returns an element from the specified array.";
const char doc87[] PROGMEM = "(assoc key list)\n"
"Looks up a key in an association list of (key . value) pairs,\n"
"and returns the matching pair, or nil if no pair is found.";
const char doc88[] PROGMEM = "(member item list)\n"
"Searches for an item in a list, using eq, and returns the list starting from the first occurrence of the item,\n"
"or nil if it is not found.";
const char doc89[] PROGMEM = "(apply function list)\n"
"Returns the result of evaluating function, with the list of arguments specified by the second parameter.";
const char doc90[] PROGMEM = "(funcall function argument*)\n"
"Evaluates function with the specified arguments.";
const char doc91[] PROGMEM = "(append list*)\n"
"Joins its arguments, which should be lists, into a single list.";
const char doc92[] PROGMEM = "(mapc function list1 [list]*)\n"
"Applies the function to each element in one or more lists, ignoring the results.\n"
"It returns the first list argument.";
const char doc93[] PROGMEM = "(mapcar function list1 [list]*)\n"
"Applies the function to each element in one or more lists, and returns the resulting list.";
const char doc94[] PROGMEM = "(mapcan function list1 [list]*)\n"
"Applies the function to each element in one or more lists. The results should be lists,\n"
"and these are appended together to give the value returned.";
const char doc95[] PROGMEM = "(+ number*)\n"
"Adds its arguments together.\n"
"If each argument is an integer, and the running total doesn't overflow, the result is an integer,\n"
"otherwise a floating-point number.";
const char doc96[] PROGMEM = "(- number*)\n"
"If there is one argument, negates the argument.\n"
"If there are two or more arguments, subtracts the second and subsequent arguments from the first argument.\n"
"If each argument is an integer, and the running total doesn't overflow, returns the result as an integer,\n"
"otherwise a floating-point number.";
const char doc97[] PROGMEM = "(* number*)\n"
"Multiplies its arguments together.\n"
"If each argument is an integer, and the running total doesn't overflow, the result is an integer,\n"
"otherwise it's a floating-point number.";
const char doc98[] PROGMEM = "(/ number*)\n"
"Divides the first argument by the second and subsequent arguments.\n"
"If each argument is an integer, and each division produces an exact result, the result is an integer;\n"
"otherwise it's a floating-point number.";
const char doc99[] PROGMEM = "(mod number number)\n"
"Returns its first argument modulo the second argument.\n"
"If both arguments are integers the result is an integer; otherwise it's a floating-point number.";
const char doc100[] PROGMEM = "(1+ number)\n"
"Adds one to its argument and returns it.\n"
"If the argument is an integer the result is an integer if possible;\n"
"otherwise it's a floating-point number.";
const char doc101[] PROGMEM = "(1- number)\n"
"Subtracts one from its argument and returns it.\n"
"If the argument is an integer the result is an integer if possible;\n"
"otherwise it's a floating-point number.";
const char doc102[] PROGMEM = "(abs number)\n"
"Returns the absolute, positive value of its argument.\n"
"If the argument is an integer the result will be returned as an integer if possible,\n"
"otherwise a floating-point number.";
const char doc103[] PROGMEM = "(random number)\n"
"If number is an integer returns a random number between 0 and one less than its argument.\n"
"Otherwise returns a floating-point number between zero and number.";
const char doc104[] PROGMEM = "(max number*)\n"
"Returns the maximum of one or more arguments.";
const char doc105[] PROGMEM = "(min number*)\n"
"Returns the minimum of one or more arguments.";
const char doc106[] PROGMEM = "(/= number*)\n"
"Returns t if none of the arguments are equal, or nil if two or more arguments are equal.";
const char doc107[] PROGMEM = "(= number*)\n"
"Returns t if all the arguments, which must be numbers, are numerically equal, and nil otherwise.";
const char doc108[] PROGMEM = "(< number*)\n"
"Returns t if each argument is less than the next argument, and nil otherwise.";
const char doc109[] PROGMEM = "(<= number*)\n"
"Returns t if each argument is less than or equal to the next argument, and nil otherwise.";
const char doc110[] PROGMEM = "(> number*)\n"
"Returns t if each argument is greater than the next argument, and nil otherwise.";
const char doc111[] PROGMEM = "(>= number*)\n"
"Returns t if each argument is greater than or equal to the next argument, and nil otherwise.";
const char doc112[] PROGMEM = "(plusp number)\n"
"Returns t if the argument is greater than zero, or nil otherwise.";
const char doc113[] PROGMEM = "(minusp number)\n"
"Returns t if the argument is less than zero, or nil otherwise.";
const char doc114[] PROGMEM = "(zerop number)\n"
"Returns t if the argument is zero.";
const char doc115[] PROGMEM = "(oddp number)\n"
"Returns t if the integer argument is odd.";
const char doc116[] PROGMEM = "(evenp number)\n"
"Returns t if the integer argument is even.";
const char doc117[] PROGMEM = "(integerp number)\n"
"Returns t if the argument is an integer.";
const char doc118[] PROGMEM = "(numberp number)\n"
"Returns t if the argument is a number.";
const char doc119[] PROGMEM = "(float number)\n"
"Returns its argument converted to a floating-point number.";
const char doc120[] PROGMEM = "(floatp number)\n"
"Returns t if the argument is a floating-point number.";
const char doc121[] PROGMEM = "(sin number)\n"
"Returns sin(number).";
const char doc122[] PROGMEM = "(cos number)\n"
"Returns cos(number).";
const char doc123[] PROGMEM = "(tan number)\n"
"Returns tan(number).";
const char doc124[] PROGMEM = "(asin number)\n"
"Returns asin(number).";
const char doc125[] PROGMEM = "(acos number)\n"
"Returns acos(number).";
const char doc126[] PROGMEM = "(atan number1 [number2])\n"
"Returns the arc tangent of number1/number2, in radians. If number2 is omitted it defaults to 1.";
const char doc127[] PROGMEM = "(sinh number)\n"
"Returns sinh(number).";
const char doc128[] PROGMEM = "(cosh number)\n"
"Returns cosh(number).";
const char doc129[] PROGMEM = "(tanh number)\n"
"Returns tanh(number).";
const char doc130[] PROGMEM = "(exp number)\n"
"Returns exp(number).";
const char doc131[] PROGMEM = "(sqrt number)\n"
"Returns sqrt(number).";
const char doc132[] PROGMEM = "(number [base])\n"
"Returns the logarithm of number to the specified base. If base is omitted it defaults to e.";
const char doc133[] PROGMEM = "(expt number power)\n"
"Returns number raised to the specified power.\n"
"Returns the result as an integer if the arguments are integers and the result will be within range,\n"
"otherwise a floating-point number.";
const char doc134[] PROGMEM = "(ceiling number [divisor])\n"
"Returns ceil(number/divisor). If omitted, divisor is 1.";
const char doc135[] PROGMEM = "(floor number [divisor])\n"
"Returns floor(number/divisor). If omitted, divisor is 1.";
const char doc136[] PROGMEM = "(truncate number)\n"
"Returns t if the argument is a floating-point number.";
const char doc137[] PROGMEM = "(round number)\n"
"Returns t if the argument is a floating-point number.";
const char doc138[] PROGMEM = "(char string n)\n"
"Returns the nth character in a string, counting from zero.";
const char doc139[] PROGMEM = "(char-code character)\n"
"Returns the ASCII code for a character, as an integer.";
const char doc140[] PROGMEM = "(code-char integer)\n"
"Returns the character for the specified ASCII code.";
const char doc141[] PROGMEM = "(characterp item)\n"
"Returns t if the argument is a character and nil otherwise.";
const char doc142[] PROGMEM = "(stringp item)\n"
"Returns t if the argument is a string and nil otherwise.";
const char doc143[] PROGMEM = "(string= string string)\n"
"Tests whether two strings are the same.";
const char doc144[] PROGMEM = "(string< string string)\n"
"Returns t if the first string is alphabetically less than the second string, and nil otherwise. For example:";
const char doc145[] PROGMEM = "(string> string string)\n"
"Returns t if the first string is alphabetically greater than the second string, and nil otherwise.";
const char doc146[] PROGMEM = "(sort list test)\n"
"Destructively sorts list according to the test function, using an insertion sort, and returns the sorted list.";
const char doc147[] PROGMEM = "(string item)\n"
"Converts its argument to a string.";
const char doc148[] PROGMEM = "(concatenate 'string string*)\n"
"Joins together the strings given in the second and subsequent arguments, and returns a single string.";
const char doc149[] PROGMEM = "(subseq string start [end])\n"
"Returns a substring from a string, from character start to character end-1:";
const char doc150[] PROGMEM = "(read-from-string string)\n"
"Reads an atom or list from the specified string and returns it.";
const char doc151[] PROGMEM = "(princ-to-string item)\n"
"Prints its argument to a string, and returns the string.\n"
"Characters and strings are printed without quotation marks or escape characters.";
const char doc152[] PROGMEM = "(prin1-to-string item [stream])\n"
"Prints its argument to a string, and returns the string.\n"
"Characters and strings are printed with quotation marks and escape characters,\n"
"in a format that will be suitable for read-from-string.";
const char doc153[] PROGMEM = "(logand [value*])\n"
"Returns the bitwise & of the values.";
const char doc154[] PROGMEM = "(logior [value*])\n"
"Returns the bitwise | of the values.";
const char doc155[] PROGMEM = "(logxor [value*])\n"
"Returns the bitwise ^ of the values.";
const char doc156[] PROGMEM = "(prin1-to-string item [stream])\n"
"Prints its argument to a string, and returns the string.\n"
"Characters and strings are printed with quotation marks and escape characters,\n"
"in a format that will be suitable for read-from-string.";
const char doc157[] PROGMEM = "(ash value shift)\n"
"Returns the result of bitwise shifting value by shift bits. If shift is positive, value is shifted to the left.";
const char doc158[] PROGMEM = "(logbitp bit value)\n"
"Returns t if bit number bit in value is a '1', and nil if it is a '0'.";
const char doc159[] PROGMEM = "(eval form*)\n"
"Evaluates its argument an extra time.";
const char doc160[] PROGMEM = "(globals)\n"
"Returns an association list of global variables and their values.";
const char doc161[] PROGMEM = "(locals)\n"
"Returns an association list of local variables and their values.";
const char doc162[] PROGMEM = "(makunbound symbol)\n"
"Removes the value of the symbol from GlobalEnv and returns the symbol.";
const char doc163[] PROGMEM = "(break)\n"
"Inserts a breakpoint in the program. When evaluated prints Break! and reenters the REPL.";
const char doc164[] PROGMEM = "(read [stream])\n"
"Reads an atom or list from the serial input and returns it.\n"
"If stream is specified the item is read from the specified stream.";
const char doc165[] PROGMEM = "(prin1 item [stream])\n"
"Prints its argument, and returns its value.\n"
"Strings are printed with quotation marks and escape characters.";
const char doc166[] PROGMEM = "(print item [stream])\n"
"Prints its argument with quotation marks and escape characters, on a new line, and followed by a space.\n"
"If stream is specified the argument is printed to the specified stream.";
const char doc167[] PROGMEM = "(princ item [stream])\n"
"Prints its argument, and returns its value.\n"
"Characters and strings are printed without quotation marks or escape characters.";
const char doc168[] PROGMEM = "(terpri [stream])\n"
"Prints a new line, and returns nil.\n"
"If stream is specified the new line is written to the specified stream.";
const char doc169[] PROGMEM = "(read-byte stream)\n"
"Reads a byte from a stream and returns it.";
const char doc170[] PROGMEM = "(read-line [stream])\n"
"Reads characters from the serial input up to a newline character, and returns them as a string, excluding the newline.\n"
"If stream is specified the line is read from the specified stream.";
const char doc171[] PROGMEM = "(write-byte number [stream])\n"
"Writes a byte to a stream.";
const char doc172[] PROGMEM = "(write-string string [stream])\n"
"Writes a string. If stream is specified the string is written to the stream.";
const char doc173[] PROGMEM = "(write-line string [stream])\n"
"Writes a string terminated by a newline character. If stream is specified the string is written to the stream.";
const char doc174[] PROGMEM = "(restart-i2c stream [read-p])\n"
"Restarts an i2c-stream.\n"
"If read-p is nil or omitted the stream is written to.\n"
"If read-p is an integer it specifies the number of bytes to be read from the stream.";
const char doc175[] PROGMEM = "(gc)\n"
"Forces a garbage collection and prints the number of objects collected, and the time taken.";
const char doc176[] PROGMEM = "(room)\n"
"Returns the number of free Lisp cells remaining.";
const char doc177[] PROGMEM = "(save-image [symbol])\n"
"Saves the current uLisp image to non-volatile memory or SD card so it can be loaded using load-image.";
const char doc178[] PROGMEM = "(load-image [filename])\n"
"Loads a saved uLisp image from non-volatile memory or SD card.";
const char doc179[] PROGMEM = "(cls)\n"
"Prints a clear-screen character.";
const char doc180[] PROGMEM = "(pinmode pin mode)\n"
"Sets the input/output mode of an Arduino pin number, and returns nil.\n"
"The mode parameter can be an integer, a keyword, or t or nil.";
const char doc181[] PROGMEM = "(digitalread pin)\n"
"Reads the state of the specified Arduino pin number and returns t (high) or nil (low).";
const char doc182[] PROGMEM = "(digitalwrite pin state)\n"
"Sets the state of the specified Arduino pin number.";
const char doc183[] PROGMEM = "(analogread pin)\n"
"Reads the specified Arduino analogue pin number and returns the value.";
const char doc184[] PROGMEM = "(analogreference keyword)\n"
"Specifies a keyword to set the analogue reference voltage used for analogue input.";
const char doc185[] PROGMEM = "(analogreadresolution bits)\n"
"Specifies the resolution for the analogue inputs on platforms that support it.\n"
"The default resolution on all platforms is 10 bits.";
const char doc186[] PROGMEM = "(analogwrite pin value)\n"
"Writes the value to the specified Arduino pin number.";
const char doc187[] PROGMEM = "(analogwrite pin value)\n"
"Sets the analogue write resolution.";
const char doc188[] PROGMEM = "(delay number)\n"
"Delays for a specified number of milliseconds.";
const char doc189[] PROGMEM = "(millis)\n"
"Returns the time in milliseconds that uLisp has been running.";
const char doc190[] PROGMEM = "(sleep secs)\n"
"Puts the processor into a low-power sleep mode for secs.\n"
"Only supported on some platforms. On other platforms it does delay(1000*secs).";
const char doc191[] PROGMEM = "(note [pin] [note] [octave])\n"
"Generates a square wave on pin.\n"
"The argument note represents the note in the well-tempered scale, from 0 to 11,\n"
"where 0 represents C, 1 represents C#, and so on.\n"
"The argument octave can be from 3 to 6. If omitted it defaults to 0.";
const char doc192[] PROGMEM = "(register address [value])\n"
"Reads or writes the value of a peripheral register.\n"
"If value is not specified the function returns the value of the register at address.\n"
"If value is specified the value is written to the register at address and the function returns value.";
const char doc193[] PROGMEM = "(edit 'function)\n"
"Calls the Lisp tree editor to allow you to edit a function definition.";
const char doc194[] PROGMEM = "(pprint item [str])\n"
"Prints its argument, using the pretty printer, to display it formatted in a structured way.\n"
"If str is specified it prints to the specified stream. It returns no value.";
const char doc195[] PROGMEM = "(pprintall [str])\n"
"Pretty-prints the definition of every function and variable defined in the uLisp workspace.\n"
"Is str is specified it prints to the specified stream. It returns no value.";
const char doc196[] PROGMEM = "(format output controlstring arguments*)\n"
"Outputs its arguments formatted according to the format directives in controlstring.";
const char doc197[] PROGMEM = "(require 'symbol)\n"
"Loads the definition of a function defined with defun, or a variable defined with defvar, from the Lisp Library.\n"
"It returns t if it was loaded, or nil if the symbol is already defined or isn't defined in the Lisp Library.";
const char doc198[] PROGMEM = "(list-library)\n"
"Prints a list of the functions defined in the List Library.";
const char doc199[] PROGMEM = "(documentation 'symbol [type])\n"
"Returns the documentation string of a built-in or user-defined function. The type argument is ignored.";
const char doc200[] PROGMEM = "(available stream)\n"
"Returns the number of bytes available for reading from the wifi-stream, or zero if no bytes are available.";
const char doc201[] PROGMEM = "(wifi-server)\n"
"Starts a Wi-Fi server running. It returns nil.";
const char doc202[] PROGMEM = "(wifi-softap ssid [password channel hidden])\n"
"Set up a soft access point to establish a Wi-Fi network.\n"
"Returns the IP address as a string or nil if unsuccessful.";
const char doc203[] PROGMEM = "(connected stream)\n"
"Returns t or nil to indicate if the client on stream is connected.";
const char doc204[] PROGMEM = "(wifi-localip)\n"
"Returns the IP address of the local network as a string.";
const char doc205[] PROGMEM = "(wifi-connect [ssid pass])\n"
"Connects to the Wi-Fi network ssid using password pass. It returns the IP address as a string.";
const char doc206[] PROGMEM = "(draw-pixel x y [colour])\n"
"Draws a pixel at coordinates (x,y) in colour, or white if omitted.";
const char doc207[] PROGMEM = "(draw-line x0 y0 x1 y1 [colour])\n"
"Draws a line from (x0,y0) to (x1,y1) in colour, or white if omitted.";
const char doc208[] PROGMEM = "(draw-rect x y w h [colour])\n"
"Draws an outline rectangle with its top left corner at (x,y), with width w,\n"
"and with height h. The outline is drawn in colour, or white if omitted.";
const char doc209[] PROGMEM = "(fill-rect x y w h [colour])\n"
"Draws a filled rectangle with its top left corner at (x,y), with width w,\n"
"and with height h. The outline is drawn in colour, or white if omitted.";
const char doc210[] PROGMEM = "(draw-circle x y r [colour])\n"
"Draws an outline circle with its centre at (x, y) and with radius r.\n"
"The circle is drawn in colour, or white if omitted.";
const char doc211[] PROGMEM = "(fill-circle x y r [colour])\n"
"Draws a filled circle with its centre at (x, y) and with radius r.\n"
"The circle is drawn in colour, or white if omitted.";
const char doc212[] PROGMEM = "(draw-round-rect x y w h radius [colour])\n"
"Draws an outline rounded rectangle with its top left corner at (x,y), with width w,\n"
"height h, and corner radius radius. The outline is drawn in colour, or white if omitted.";
const char doc213[] PROGMEM = "(fill-round-rect x y w h radius [colour])\n"
"Draws a filled rounded rectangle with its top left corner at (x,y), with width w,\n"
"height h, and corner radius radius. The outline is drawn in colour, or white if omitted.";
const char doc214[] PROGMEM = "(draw-triangle x0 y0 x1 y1 x2 y2 [colour])\n"
"Draws an outline triangle between (x1,y1), (x2,y2), and (x3,y3).\n"
"The outline is drawn in colour, or white if omitted.";
const char doc215[] PROGMEM = "(fill-triangle x0 y0 x1 y1 x2 y2 [colour])\n"
"Draws a filled triangle between (x1,y1), (x2,y2), and (x3,y3).\n"
"The outline is drawn in colour, or white if omitted.";
const char doc216[] PROGMEM = "(draw-char x y char [colour background size])\n"
"Draws the character char with its top left corner at (x,y).\n"
"The character is drawn in a 5 x 7 pixel font in colour against background,\n"
"which default to white and black respectively.\n"
"The character can optionally be scaled by size.";
const char doc217[] PROGMEM = "(set-cursor x y)\n"
"Sets the start point for text plotting to (x, y).";
const char doc218[] PROGMEM = "(set-text-color colour [background])\n"
"Sets the text colour for text plotted using (with-gfx ...).";
const char doc219[] PROGMEM = "(set-text-size scale)\n"
"Scales text by the specified size, default 1.";
const char doc220[] PROGMEM = "(set-text-wrap boolean)\n"
"Specified whether text wraps at the right-hand edge of the display; the default is t.";
const char doc221[] PROGMEM = "(fill-screen [colour])\n"
"Fills or clears the screen with colour, default black.";
const char doc222[] PROGMEM = "(set-rotation option)\n"
"Sets the display orientation for subsequent graphics commands; values are 0, 1, 2, or 3.";
const char doc223[] PROGMEM = "(invert-display boolean)\n"
"Mirror-images the display.";

// Insert your own function documentation here
const char mydoc[] PROGMEM = "(refresh)\n"
"Triggers a refresh of the display.";

const char mydoc2[] PROGMEM = "(get-key)\n"
"Returns a key event, or nil if there are no key events in the queue.";

const char mydoc5[] PROGMEM = "(subseql list start [end])\n"
"Returns a subsequence of the list, from start to end";

const char peekdoc[] PROGMEM = "(peek address)\n"
"Returns the contents of the specified memory address.";
const char pokedoc[] PROGMEM = "(poke address value)\n"
"Stores value in the specified memory address, and returns value.";

// Built-in symbol lookup table
const tbl_entry_t lookup_table[] PROGMEM = {
  { string0, NULL, 0x00, doc0 },
  { string1, NULL, 0x00, doc1 },
  { string2, NULL, 0x00, doc2 },
  { string3, NULL, 0x00, doc3 },
  { string4, NULL, 0x00, NULL },
  { string5, NULL, 0x00, NULL },
  { string6, NULL, 0x00, NULL },
  { string7, NULL, 0x00, doc7 },
  { string8, NULL, 0x0F, doc8 },
  { string9, NULL, 0x0F, doc9 },
  { string10, NULL, 0x0F, doc10 },
  { string11, NULL, 0x0F, NULL },
  { string12, NULL, 0x0F, NULL },
  { string13, NULL, 0x00, NULL },
  { string14, sp_quote, 0x11, NULL },
  { string15, sp_or, 0x0F, doc15 },
  { string16, sp_defun, 0x2F, doc16 },
  { string17, sp_defvar, 0x13, doc17 },
  { string18, sp_setq, 0x2F, doc18 },
  { string19, sp_loop, 0x0F, doc19 },
  { string20, sp_return, 0x0F, doc20 },
  { string21, sp_push, 0x22, doc21 },
  { string22, sp_pop, 0x11, doc22 },
  { string23, sp_incf, 0x12, doc23 },
  { string24, sp_decf, 0x12, doc24 },
  { string25, sp_setf, 0x2F, doc25 },
  { string26, sp_dolist, 0x1F, doc26 },
  { string27, sp_dotimes, 0x1F, doc27 },
  { string28, sp_trace, 0x01, doc28 },
  { string29, sp_untrace, 0x01, doc29 },
  { string30, sp_formillis, 0x1F, doc30 },
  { string31, sp_time, 0x11, doc31 },
  { string32, sp_withoutputtostring, 0x1F, doc32 },
  { string33, sp_withserial, 0x1F, doc33 },
  { string34, sp_withi2c, 0x1F, doc34 },
  { string35, sp_withspi, 0x1F, doc35 },
  { string36, sp_withsdcard, 0x2F, doc36 },
  { string37, sp_withgfx, 0x1F, doc37 },
  { string38, sp_withclient, 0x12, doc38 },
  { string944d81fd0ed3aa6433ad9b560576, sp_unwindprotect, 0x1F, NULL },
  { string4c085b45ade192ea2a98b36e8a24, sp_ignoreerrors, 0x0F, NULL },
  { stringda16dce31347cc2b47fc4bac32ed, sp_error, 0x0F, NULL },
  { string39, sp_defcode, 0x0F, doc39 },
  { string40, NULL, 0x00, NULL },
  { string41, tf_progn, 0x0F, doc41 },
  { string42, tf_if, 0x23, doc42 },
  { string43, tf_cond, 0x0F, doc43 },
  { string44, tf_when, 0x1F, doc44 },
  { string45, tf_unless, 0x1F, doc45 },
  { string46, tf_case, 0x1F, doc46 },
  { string47, tf_and, 0x0F, doc47 },
  { string48, tf_help, 0x11, doc48 },
  { string49, NULL, 0x00, NULL },
  { string50, fn_not, 0x11, doc50 },
  { string51, fn_not, 0x11, NULL },
  { string52, fn_cons, 0x22, doc52 },
  { string53, fn_atom, 0x11, doc53 },
  { string54, fn_listp, 0x11, doc54 },
  { string55, fn_consp, 0x11, doc55 },
  { string56, fn_symbolp, 0x11, doc56 },
  { string57, fn_arrayp, 0x11, doc57 },
  { string58, fn_boundp, 0x11, doc58 },
  { string59, fn_setfn, 0x2F, doc59 },
  { string60, fn_streamp, 0x11, doc60 },
  { string61, fn_eq, 0x22, doc61 },
  { string62, fn_car, 0x11, doc62 },
  { string63, fn_car, 0x11, NULL },
  { string64, fn_cdr, 0x11, doc64 },
  { string65, fn_cdr, 0x11, NULL },
  { string66, fn_caar, 0x11, doc66 },
  { string67, fn_cadr, 0x11, doc67 },
  { string68, fn_cadr, 0x11, NULL },
  { string69, fn_cdar, 0x11, doc69 },
  { string70, fn_cddr, 0x11, doc70 },
  { string71, fn_caaar, 0x11, doc71 },
  { string72, fn_caadr, 0x11, doc72 },
  { string73, fn_cadar, 0x11, doc73 },
  { string74, fn_caddr, 0x11, doc74 },
  { string75, fn_caddr, 0x11, NULL },
  { string76, fn_cdaar, 0x11, doc76 },
  { string77, fn_cdadr, 0x11, doc77 },
  { string78, fn_cddar, 0x11, doc78 },
  { string79, fn_cdddr, 0x11, doc79 },
  { string80, fn_length, 0x11, doc80 },
  { string81, fn_arraydimensions, 0x11, doc81 },
  { string82, fn_list, 0x0F, doc82 },
  { string83, fn_makearray, 0x15, doc83 },
  { string84, fn_reverse, 0x11, doc84 },
  { string85, fn_nth, 0x22, doc85 },
  { string86, fn_aref, 0x2F, doc86 },
  { string87, fn_assoc, 0x22, doc87 },
  { string88, fn_member, 0x22, doc88 },
  { string89, fn_apply, 0x2F, doc89 },
  { string90, fn_funcall, 0x1F, doc90 },
  { string91, fn_append, 0x0F, doc91 },
  { string92, fn_mapc, 0x2F, doc92 },
  { string93, fn_mapcar, 0x2F, doc93 },
  { string94, fn_mapcan, 0x2F, doc94 },
  { string95, fn_add, 0x0F, doc95 },
  { string96, fn_subtract, 0x1F, doc96 },
  { string97, fn_multiply, 0x0F, doc97 },
  { string98, fn_divide, 0x1F, doc98 },
  { string99, fn_mod, 0x22, doc99 },
  { string100, fn_oneplus, 0x11, doc100 },
  { string101, fn_oneminus, 0x11, doc101 },
  { string102, fn_abs, 0x11, doc102 },
  { string103, fn_random, 0x11, doc103 },
  { string104, fn_maxfn, 0x1F, doc104 },
  { string105, fn_minfn, 0x1F, doc105 },
  { string106, fn_noteq, 0x1F, doc106 },
  { string107, fn_numeq, 0x1F, doc107 },
  { string108, fn_less, 0x1F, doc108 },
  { string109, fn_lesseq, 0x1F, doc109 },
  { string110, fn_greater, 0x1F, doc110 },
  { string111, fn_greatereq, 0x1F, doc111 },
  { string112, fn_plusp, 0x11, doc112 },
  { string113, fn_minusp, 0x11, doc113 },
  { string114, fn_zerop, 0x11, doc114 },
  { string115, fn_oddp, 0x11, doc115 },
  { string116, fn_evenp, 0x11, doc116 },
  { string117, fn_integerp, 0x11, doc117 },
  { string118, fn_numberp, 0x11, doc118 },
  { string119, fn_floatfn, 0x11, doc119 },
  { string120, fn_floatp, 0x11, doc120 },
  { string121, fn_sin, 0x11, doc121 },
  { string122, fn_cos, 0x11, doc122 },
  { string123, fn_tan, 0x11, doc123 },
  { string124, fn_asin, 0x11, doc124 },
  { string125, fn_acos, 0x11, doc125 },
  { string126, fn_atan, 0x12, doc126 },
  { string127, fn_sinh, 0x11, doc127 },
  { string128, fn_cosh, 0x11, doc128 },
  { string129, fn_tanh, 0x11, doc129 },
  { string130, fn_exp, 0x11, doc130 },
  { string131, fn_sqrt, 0x11, doc131 },
  { string132, fn_log, 0x12, doc132 },
  { string133, fn_expt, 0x22, doc133 },
  { string134, fn_ceiling, 0x12, doc134 },
  { string135, fn_floor, 0x12, doc135 },
  { string136, fn_truncate, 0x12, doc136 },
  { string137, fn_round, 0x12, doc137 },
  { string138, fn_char, 0x22, doc138 },
  { string139, fn_charcode, 0x11, doc139 },
  { string140, fn_codechar, 0x11, doc140 },
  { string141, fn_characterp, 0x11, doc141 },
  { string142, fn_stringp, 0x11, doc142 },
  { string143, fn_stringeq, 0x22, doc143 },
  { string144, fn_stringless, 0x22, doc144 },
  { string145, fn_stringgreater, 0x22, doc145 },
  { string146, fn_sort, 0x22, doc146 },
  { string147, fn_stringfn, 0x11, doc147 },
  { string148, fn_concatenate, 0x1F, doc148 },
  { string149, fn_subseq, 0x23, doc149 },
  { string150, fn_readfromstring, 0x11, doc150 },
  { string151, fn_princtostring, 0x11, doc151 },
  { string152, fn_prin1tostring, 0x11, doc152 },
  { string153, fn_logand, 0x0F, doc153 },
  { string154, fn_logior, 0x0F, doc154 },
  { string155, fn_logxor, 0x0F, doc155 },
  { string156, fn_lognot, 0x11, doc156 },
  { string157, fn_ash, 0x22, doc157 },
  { string158, fn_logbitp, 0x22, doc158 },
  { string159, fn_eval, 0x11, doc159 },
  { string160, fn_globals, 0x00, doc160 },
  { string161, fn_locals, 0x00, doc161 },
  { string162, fn_makunbound, 0x11, doc162 },
  { string163, fn_break, 0x00, doc163 },
  { string164, fn_read, 0x01, doc164 },
  { string165, fn_prin1, 0x12, doc165 },
  { string166, fn_print, 0x12, doc166 },
  { string167, fn_princ, 0x12, doc167 },
  { string168, fn_terpri, 0x01, doc168 },
  { string169, fn_readbyte, 0x02, doc169 },
  { string170, fn_readline, 0x01, doc170 },
  { string171, fn_writebyte, 0x12, doc171 },
  { string172, fn_writestring, 0x12, doc172 },
  { string173, fn_writeline, 0x12, doc173 },
  { string174, fn_restarti2c, 0x12, doc174 },
  { string175, fn_gc, 0x00, doc175 },
  { string176, fn_room, 0x00, doc176 },
  { string177, fn_saveimage, 0x01, doc177 },
  { string178, fn_loadimage, 0x01, doc178 },
  { string179, fn_cls, 0x00, doc179 },
  { string180, fn_pinmode, 0x22, doc180 },
  { string181, fn_digitalread, 0x11, doc181 },
  { string182, fn_digitalwrite, 0x22, doc182 },
  { string183, fn_analogread, 0x11, doc183 },
  { string184, fn_analogreference, 0x11, doc184 },
  { string185, fn_analogreadresolution, 0x11, doc185 },
  { string186, fn_analogwrite, 0x22, doc186 },
  { string187, fn_analogwriteresolution, 0x11, doc187 },
  { string188, fn_delay, 0x11, doc188 },
  { string189, fn_millis, 0x00, doc189 },
  { string190, fn_sleep, 0x11, doc190 },
  { string191, fn_note, 0x03, doc191 },
  { string192, fn_register, 0x12, doc192 },
  { string193, fn_edit, 0x11, doc193 },
  { string194, fn_pprint, 0x12, doc194 },
  { string195, fn_pprintall, 0x01, doc195 },
  { string196, fn_format, 0x2F, doc196 },
  { string197, fn_require, 0x11, doc197 },
  { string198, fn_listlibrary, 0x00, doc198 },
  { string199, fn_documentation, 0x12, doc199 },
  { string200, fn_available, 0x11, doc200 },
  { string201, fn_wifiserver, 0x00, doc201 },
  { string202, fn_wifisoftap, 0x04, doc202 },
  { string203, fn_connected, 0x11, doc203 },
  { string204, fn_wifilocalip, 0x00, doc204 },
  { string205, fn_wificonnect, 0x03, doc205 },
  { string206, fn_drawpixel, 0x23, doc206 },
  { string207, fn_drawline, 0x45, doc207 },
  { string208, fn_drawrect, 0x45, doc208 },
  { string209, fn_fillrect, 0x45, doc209 },
  { string210, fn_drawcircle, 0x34, doc210 },
  { string211, fn_fillcircle, 0x34, doc211 },
  { string212, fn_drawroundrect, 0x56, doc212 },
  { string213, fn_fillroundrect, 0x56, doc213 },
  { string214, fn_drawtriangle, 0x67, doc214 },
  { string215, fn_filltriangle, 0x67, doc215 },
  { string216, fn_drawchar, 0x36, doc216 },
  { string217, fn_setcursor, 0x22, doc217 },
  { string218, fn_settextcolor, 0x12, doc218 },
  { string219, fn_settextsize, 0x11, doc219 },
  { string220, fn_settextwrap, 0x11, doc220 },
  { string221, fn_fillscreen, 0x01, doc221 },
  { string222, fn_setrotation, 0x11, doc222 },
  { string223, fn_invertdisplay, 0x11, doc223 },
  { string224, NULL, 0x00, NULL },
  { string225, (fn_ptr_type)0x5, 0, NULL }, // Replaced LED_BUILTIN with 0x5 to silence warning
  { string226, (fn_ptr_type)HIGH, DIGITALWRITE, NULL },
  { string227, (fn_ptr_type)LOW, DIGITALWRITE, NULL },
#if defined(CPU_APOLLO3)
  { string228, (fn_ptr_type)INPUT, PINMODE },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE },
  { string231, (fn_ptr_type)OUTPUT, PINMODE },
  { string232, NULL, 0x00 },
#elif defined(CPU_ATSAMD21)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)AR_DEFAULT, ANALOGREFERENCE, NULL },
  { string233, (fn_ptr_type)AR_INTERNAL1V0, ANALOGREFERENCE, NULL },
  { string234, (fn_ptr_type)AR_INTERNAL1V65, ANALOGREFERENCE, NULL },
  { string235, (fn_ptr_type)AR_INTERNAL2V23, ANALOGREFERENCE, NULL },
  { string236, (fn_ptr_type)AR_EXTERNAL, ANALOGREFERENCE, NULL },
  { string237, (fn_ptr_type)&PORT->Group[0].DIR.reg, REGISTER, NULL },
  { string238, (fn_ptr_type)&PORT->Group[0].DIRCLR.reg, REGISTER, NULL },
  { string239, (fn_ptr_type)&PORT->Group[0].DIRSET.reg, REGISTER, NULL },
  { string240, (fn_ptr_type)&PORT->Group[0].DIRTGL.reg, REGISTER, NULL },
  { string241, (fn_ptr_type)&PORT->Group[0].OUT.reg, REGISTER, NULL },
  { string242, (fn_ptr_type)&PORT->Group[0].OUTCLR.reg, REGISTER, NULL },
  { string243, (fn_ptr_type)&PORT->Group[0].OUTSET.reg, REGISTER, NULL },
  { string244, (fn_ptr_type)&PORT->Group[0].OUTTGL.reg, REGISTER, NULL },
  { string245, (fn_ptr_type)&PORT->Group[0].IN.reg, REGISTER, NULL },
  { string246, (fn_ptr_type)&PORT->Group[1].DIR.reg, REGISTER, NULL },
  { string247, (fn_ptr_type)&PORT->Group[1].DIRCLR.reg, REGISTER, NULL },
  { string248, (fn_ptr_type)&PORT->Group[1].DIRSET.reg, REGISTER, NULL },
  { string249, (fn_ptr_type)&PORT->Group[1].DIRTGL.reg, REGISTER, NULL },
  { string250, (fn_ptr_type)&PORT->Group[1].OUT.reg, REGISTER, NULL },
  { string251, (fn_ptr_type)&PORT->Group[1].OUTCLR.reg, REGISTER, NULL },
  { string252, (fn_ptr_type)&PORT->Group[1].OUTSET.reg, REGISTER, NULL },
  { string253, (fn_ptr_type)&PORT->Group[1].OUTTGL.reg, REGISTER, NULL },
  { string254, (fn_ptr_type)&PORT->Group[1].IN.reg, REGISTER, NULL },
  { string255, NULL, 0x00, NULL },
#elif defined(CPU_ATSAMD51)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)AR_DEFAULT, ANALOGREFERENCE, NULL },
  { string233, (fn_ptr_type)AR_INTERNAL1V0, ANALOGREFERENCE, NULL },
  { string234, (fn_ptr_type)AR_INTERNAL1V1, ANALOGREFERENCE, NULL },
  { string235, (fn_ptr_type)AR_INTERNAL1V2, ANALOGREFERENCE, NULL },
  { string236, (fn_ptr_type)AR_INTERNAL1V25, ANALOGREFERENCE, NULL },
  { string237, (fn_ptr_type)AR_INTERNAL1V65, ANALOGREFERENCE, NULL },
  { string238, (fn_ptr_type)AR_INTERNAL2V0, ANALOGREFERENCE, NULL },
  { string239, (fn_ptr_type)AR_INTERNAL2V2, ANALOGREFERENCE, NULL },
  { string240, (fn_ptr_type)AR_INTERNAL2V23, ANALOGREFERENCE, NULL },
  { string241, (fn_ptr_type)AR_INTERNAL2V4, ANALOGREFERENCE, NULL },
  { string242, (fn_ptr_type)AR_INTERNAL2V5, ANALOGREFERENCE, NULL },
  { string243, (fn_ptr_type)AR_EXTERNAL, ANALOGREFERENCE, NULL },
  { string244, (fn_ptr_type)&PORT->Group[0].DIR.reg, REGISTER, NULL },
  { string245, (fn_ptr_type)&PORT->Group[0].DIRCLR.reg, REGISTER, NULL },
  { string246, (fn_ptr_type)&PORT->Group[0].DIRSET.reg, REGISTER, NULL },
  { string247, (fn_ptr_type)&PORT->Group[0].DIRTGL.reg, REGISTER, NULL },
  { string248, (fn_ptr_type)&PORT->Group[0].OUT.reg, REGISTER, NULL },
  { string249, (fn_ptr_type)&PORT->Group[0].OUTCLR.reg, REGISTER, NULL },
  { string250, (fn_ptr_type)&PORT->Group[0].OUTSET.reg, REGISTER, NULL },
  { string251, (fn_ptr_type)&PORT->Group[0].OUTTGL.reg, REGISTER, NULL },
  { string252, (fn_ptr_type)&PORT->Group[0].IN.reg, REGISTER, NULL },
  { string253, (fn_ptr_type)&PORT->Group[1].DIR.reg, REGISTER, NULL },
  { string254, (fn_ptr_type)&PORT->Group[1].DIRCLR.reg, REGISTER, NULL },
  { string255, (fn_ptr_type)&PORT->Group[1].DIRSET.reg, REGISTER, NULL },
  { string256, (fn_ptr_type)&PORT->Group[1].DIRTGL.reg, REGISTER, NULL },
  { string257, (fn_ptr_type)&PORT->Group[1].OUT.reg, REGISTER, NULL },
  { string258, (fn_ptr_type)&PORT->Group[1].OUTCLR.reg, REGISTER, NULL },
  { string259, (fn_ptr_type)&PORT->Group[1].OUTSET.reg, REGISTER, NULL },
  { string260, (fn_ptr_type)&PORT->Group[1].OUTTGL.reg, REGISTER, NULL },
  { string261, (fn_ptr_type)&PORT->Group[1].IN.reg, REGISTER, NULL },
  { string262, NULL, 0x00, NULL },
#elif defined(CPU_NRF51822)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)AR_DEFAULT, ANALOGREFERENCE, NULL },
  { string233, (fn_ptr_type)AR_VBG, ANALOGREFERENCE, NULL },
  { string234, (fn_ptr_type)AR_SUPPLY_ONE_HALF, ANALOGREFERENCE, NULL },
  { string235, (fn_ptr_type)AR_SUPPLY_ONE_THIRD, ANALOGREFERENCE, NULL },
  { string236, (fn_ptr_type)AR_EXT0, ANALOGREFERENCE, NULL },
  { string237, (fn_ptr_type)AR_EXT1, ANALOGREFERENCE, NULL },
  { string238, (fn_ptr_type)&NRF_GPIO->OUT, REGISTER, NULL },
  { string239, (fn_ptr_type)&NRF_GPIO->OUTSET, REGISTER, NULL },
  { string240, (fn_ptr_type)&NRF_GPIO->OUTCLR, REGISTER, NULL },
  { string241, (fn_ptr_type)&NRF_GPIO->IN, REGISTER, NULL },
  { string242, (fn_ptr_type)&NRF_GPIO->DIR, REGISTER, NULL },
  { string243, (fn_ptr_type)&NRF_GPIO->DIRSET, REGISTER, NULL },
  { string244, (fn_ptr_type)&NRF_GPIO->DIRCLR, REGISTER, NULL },
  { string245, NULL, 0x00, NULL },
#elif defined(CPU_NRF52840)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)AR_DEFAULT, ANALOGREFERENCE, NULL },
  { string233, (fn_ptr_type)AR_INTERNAL, ANALOGREFERENCE, NULL },
  { string234, (fn_ptr_type)AR_INTERNAL_3_0, ANALOGREFERENCE, NULL },
  { string235, (fn_ptr_type)AR_INTERNAL_2_4, ANALOGREFERENCE, NULL },
  { string236, (fn_ptr_type)AR_INTERNAL_1_8, ANALOGREFERENCE, NULL },
  { string237, (fn_ptr_type)AR_INTERNAL_1_2, ANALOGREFERENCE, NULL },
  { string238, (fn_ptr_type)AR_VDD4, ANALOGREFERENCE, NULL },
  { string239, (fn_ptr_type)&NRF_P0->OUT, REGISTER, NULL },
  { string240, (fn_ptr_type)&NRF_P0->OUTSET, REGISTER, NULL },
  { string241, (fn_ptr_type)&NRF_P0->OUTCLR, REGISTER, NULL },
  { string242, (fn_ptr_type)&NRF_P0->IN, REGISTER, NULL },
  { string243, (fn_ptr_type)&NRF_P0->DIR, REGISTER, NULL },
  { string244, (fn_ptr_type)&NRF_P0->DIRSET, REGISTER, NULL },
  { string245, (fn_ptr_type)&NRF_P0->DIRCLR, REGISTER, NULL },
  { string246, (fn_ptr_type)&NRF_P1->OUT, REGISTER, NULL },
  { string247, (fn_ptr_type)&NRF_P1->OUTSET, REGISTER, NULL },
  { string248, (fn_ptr_type)&NRF_P1->OUTCLR, REGISTER, NULL },
  { string249, (fn_ptr_type)&NRF_P1->IN, REGISTER, NULL },
  { string250, (fn_ptr_type)&NRF_P1->DIR, REGISTER, NULL },
  { string251, (fn_ptr_type)&NRF_P1->DIRSET, REGISTER, NULL },
  { string252, (fn_ptr_type)&NRF_P1->DIRCLR, REGISTER, NULL },
  { string253, NULL, 0x00, NULL },
#elif defined(CPU_NRF52833)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)AR_DEFAULT, ANALOGREFERENCE, NULL },
  { string233, (fn_ptr_type)AR_INTERNAL, ANALOGREFERENCE, NULL },
  { string234, (fn_ptr_type)AR_VDD4, ANALOGREFERENCE, NULL },
  { string235, (fn_ptr_type)&NRF_P0->OUT, REGISTER, NULL },
  { string236, (fn_ptr_type)&NRF_P0->OUTSET, REGISTER, NULL },
  { string237, (fn_ptr_type)&NRF_P0->OUTCLR, REGISTER, NULL },
  { string238, (fn_ptr_type)&NRF_P0->IN, REGISTER, NULL },
  { string239, (fn_ptr_type)&NRF_P0->DIR, REGISTER, NULL },
  { string240, (fn_ptr_type)&NRF_P0->DIRSET, REGISTER, NULL },
  { string241, (fn_ptr_type)&NRF_P0->DIRCLR, REGISTER, NULL },
  { string242, (fn_ptr_type)&NRF_P1->OUT, REGISTER, NULL },
  { string243, (fn_ptr_type)&NRF_P1->OUTSET, REGISTER, NULL },
  { string244, (fn_ptr_type)&NRF_P1->OUTCLR, REGISTER, NULL },
  { string245, (fn_ptr_type)&NRF_P1->IN, REGISTER, NULL },
  { string246, (fn_ptr_type)&NRF_P1->DIR, REGISTER, NULL },
  { string247, (fn_ptr_type)&NRF_P1->DIRSET, REGISTER, NULL },
  { string248, (fn_ptr_type)&NRF_P1->DIRCLR, REGISTER, NULL },
  { string249, NULL, 0x00, NULL },
#elif defined(CPU_iMXRT1062)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)OUTPUT_OPENDRAIN, PINMODE, NULL },
  { string233, NULL, 0x00, NULL },
#elif defined(CPU_MAX32620)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string231, (fn_ptr_type)DEFAULT, ANALOGREFERENCE, NULL },
  { string232, (fn_ptr_type)EXTERNAL, ANALOGREFERENCE, NULL },
  { string233, NULL, 0x00, NULL },
#elif defined(CPU_RP2040)
  { string228, (fn_ptr_type)INPUT, PINMODE, NULL },
  { string229, (fn_ptr_type)INPUT_PULLUP, PINMODE, NULL },
  { string230, (fn_ptr_type)INPUT_PULLDOWN, PINMODE, NULL },
  { string231, (fn_ptr_type)OUTPUT, PINMODE, NULL },
  { string232, (fn_ptr_type)(SIO_BASE+SIO_GPIO_IN_OFFSET), REGISTER, NULL },
  { string233, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OUT_OFFSET), REGISTER, NULL },
  { string234, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OUT_SET_OFFSET), REGISTER, NULL },
  { string235, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OUT_CLR_OFFSET), REGISTER, NULL },
  { string236, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OUT_XOR_OFFSET), REGISTER, NULL },
  { string237, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OE_OFFSET), REGISTER, NULL },
  { string238, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OE_SET_OFFSET), REGISTER, NULL },
  { string239, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OE_CLR_OFFSET), REGISTER, NULL },
  { string240, (fn_ptr_type)(SIO_BASE+SIO_GPIO_OE_XOR_OFFSET), REGISTER, NULL },
  { string241, NULL, 0x00, NULL },
#endif

// Insert your own table entries here
  { user06975b647a442ae56f6ee0abc8fb, fn_geterror, 0x00, NULL },
  { mystring1, fn_refresh, 0x00, mydoc },
  { mystring2, fn_getkey, 0x00, mydoc2 },
  { mystring3, fn_peek, 0x11, peekdoc },
  { mystring4, fn_poke, 0x22, pokedoc },
  { mystring5, fn_subseql, 0x22, mydoc5 }
};

// Table lookup functions

builtin_t lookupbuiltin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
      return (builtin_t)entry;
    entry++;
  }
  return ENDFUNCTIONS;
}

intptr_t lookupfn (builtin_t name) {
  return (intptr_t)lookup_table[name].fptr;
}

uint8_t getminmax (builtin_t name) {
  uint8_t minmax = lookup_table[name].minmax;
  return minmax;
}

void checkminmax (builtin_t name, int nargs) {
  uint8_t minmax = getminmax(name);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

char *lookupdoc (builtin_t name) {
  return (char*)lookup_table[name].doc;
}

void testescape () {
  int pos = (KEYEVENT_BUFFER_SIZE + keyEventWritePtr-1)%KEYEVENT_BUFFER_SIZE;
  uint32_t lastKeyEvent = keyEvents[pos];
  if ( lastKeyEvent == 4123 ) { // Fn + Esc
    keyEvents[pos] = 0;
    error2(NIL, PSTR("escape key"));
  } else if (Serial.read() == '~') error2(NIL, PSTR("serial escape"));
}

// Main evaluator

#if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
#define ENDSTACK _ebss
#else
#define ENDSTACK end
#endif

extern uint32_t ENDSTACK;  // Bottom of stack

uint8_t End;

object *eval (object *form, object *env) {
  register int *sp asm ("r13");
  int TC=0;
  EVAL:
  // Enough space?
  // Serial.println((uint32_t)sp - (uint32_t)&ENDSTACK); // Find best STACKDIFF value
  if (((uint32_t)sp - (uint32_t)&ENDSTACK) < STACKDIFF) error2(NIL, PSTR("stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(NIL, PSTR("escape!"));}
  if (!tstflag(NOESC)) testescape();

  if (form == NULL) return nil;

  if (form->type >= NUMBER && form->type <= STRING) return form;

  if (symbolp(form)) {
    symbol_t name = form->name;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (builtinp(name)) return form;
    error(NIL, PSTR("undefined"), form);
  }

  #if defined(CODESIZE)
  if (form->type == CODE) error2(NIL, PSTR("can't evaluate CODE header"));
  #endif

  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  if (function == NULL) error(NIL, PSTR("illegal function"), nil);
  if (!listp(args)) error(NIL, PSTR("can't evaluate a dotted pair"), args);

  // List starts with a symbol?
  if (symbolp(function)) {
    builtin_t name = builtin(function->name);

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      if (args == NULL) error2(name, noargument);
      object *assigns = first(args);
      if (!listp(assigns)) error(name, notalist, assigns);
      object *forms = cdr(args);
      object *newenv = env;
      push(newenv, GCStack);
      while (assigns != NULL) {
        object *assign = car(assigns);
        if (!consp(assign)) push(cons(assign,nil), newenv);
        else if (cdr(assign) == NULL) push(cons(first(assign),nil), newenv);
        else push(cons(first(assign),eval(second(assign),env)), newenv);
        car(GCStack) = newenv;
        if (name == LETSTAR) env = newenv;
        assigns = cdr(assigns);
      }
      env = newenv;
      pop(GCStack);
      form = tf_progn(forms,env);
      TC = TCstart;
      goto EVAL;
    }

    if (name == LAMBDA) {
      if (env == NULL) return form;
      object *envcopy = NULL;
      while (env != NULL) {
        object *pair = first(env);
        if (pair != NULL) push(pair, envcopy);
        env = cdr(env);
      }
      return cons(bsymbol(CLOSURE), cons(envcopy,args));
    }

    if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }
    if (((name > 0) && (name < SPECIAL_FORMS)) || ((name > KEYWORDS) && (name < USERFUNCTIONS))) error2(name, PSTR("can't be used as a function"));
  }

  // Evaluate the parameters - result in head
  object *fname = car(form);
  int TCstart = TC;
  object *head = cons(eval(fname, env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL){
    object *obj = cons(eval(car(form),env),NULL);
    cdr(tail) = obj;
    tail = obj;
    form = cdr(form);
    nargs++;
  }

  function = car(head);
  args = cdr(head);

  if (symbolp(function)) {
    builtin_t bname = builtin(function->name);
    if (!builtinp(function->name)) error(NIL, PSTR("not valid here"), fname);
    checkminmax(bname, nargs);
    object *result = ((fn_ptr_type)lookupfn(bname))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function)) {
    symbol_t name = sym(NIL);
    if (!listp(fname)) name = fname->name;

    if (isbuiltin(car(function), LAMBDA)) {
      form = closure(TCstart, name, function, args, &env);
      pop(GCStack);
      int trace = tracing(fname->name);
      if (trace) {
        object *result = eval(form, env);
        indent((--(TraceDepth[trace-1]))<<1, ' ', pserial);
        pint(TraceDepth[trace-1], pserial);
        pserial(':'); pserial(' ');
        printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
        printobject(result, pserial); pln(pserial);
        return result;
      } else {
        TC = 1;
        goto EVAL;
      }
    }

    if (isbuiltin(car(function), CLOSURE)) {
      function = cdr(function);
      form = closure(TCstart, name, function, args, &env);
      pop(GCStack);
      TC = 1;
      goto EVAL;
    }

    if (car(function)->type == CODE) {
      int n = listlength(DEFCODE, second(function));
      if (nargs<n) errorsym2(fname->name, toofewargs);
      if (nargs>n) errorsym2(fname->name, toomanyargs);
      uint32_t entry = startblock(car(function)) + 1;
      pop(GCStack);
      return call(entry, n, args, env);
    }

  }
  error(NIL, PSTR("illegal function"), fname); return nil;
}

// Print functions

void pserial (char c) {
  LastPrint = c;
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}

const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

void pcharacter (uint8_t c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c <= 32) {
      const char *p = ControlCodes;
      while (c > 0) {p = p + strlen(p) + 1; c--; }
      pfstring(p, pfun);
    } else if (c < 127) pfun(c);
    else pint(c, pfun);
  }
}

void pstring (char *s, pfun_t pfun) {
  while (*s) pfun(*s++);
}

void plispstring (object *form, pfun_t pfun) {
  plispstr(form->name, pfun);
}

void plispstr (symbol_t name, pfun_t pfun) {
  object *form = (object *)name;
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (tstflag(PRINTREADABLY) && (ch == '"' || ch == '\\')) pfun('\\');
      if (ch) pfun(ch);
    }
    form = car(form);
  }
}

void printstring (object *form, pfun_t pfun) {
  if (tstflag(PRINTREADABLY)) pfun('"');
  plispstr(form->name, pfun);
  if (tstflag(PRINTREADABLY)) pfun('"');
}

void pbuiltin (builtin_t name, pfun_t pfun) {
  int p = 0;
  const char *s = lookup_table[name].string;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}

void pradix40 (symbol_t name, pfun_t pfun) {
  uint32_t x = untwist(name);
  for (int d=102400000; d>0; d = d/40) {
    uint32_t j = x/d;
    char c = fromradix40(j);
    if (c == 0) return;
    pfun(c); x = x - j*d;
  }
}

void printsymbol (object *form, pfun_t pfun) {
  psymbol(form->name, pfun);
}

void psymbol (symbol_t name, pfun_t pfun) {
  if ((name & 0x03) == 0) plispstr(name, pfun);
  else {
    uint32_t value = untwist(name);
    if (value < PACKEDS) error2(NIL, PSTR("invalid symbol"));
    else if (value >= BUILTINS) pbuiltin((builtin_t)(value-BUILTINS), pfun);
    else pradix40(name, pfun);
  }
}

void pfstring (const char *s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}

void pint (int i, pfun_t pfun) {
  uint32_t j = i;
  if (i<0) { pfun('-'); j=-i; }
  pintbase(j, 10, pfun);
}

void pintbase (uint32_t i, uint8_t base, pfun_t pfun) {
  int lead = 0; uint32_t p = 1000000000;
  if (base == 2) p = 0x80000000; else if (base == 16) p = 0x10000000;
  for (uint32_t d=p; d>0; d=d/base) {
    uint32_t j = i/d;
    if (j!=0 || lead || d==1) { pfun((j<10) ? j+'0' : j+'W'); lead=1;}
    i = i - j*d;
  }
}

void printhex4 (int i, pfun_t pfun) {
  int p = 0x1000;
  for (int d=p; d>0; d=d/16) {
    int j = i/d;
    pfun((j<10) ? j+'0' : j + 'W');
    i = i - j*d;
  }
  pfun(' ');
}

void pmantissa (float f, pfun_t pfun) {
  int sig = floor(log10(f));
  int mul = pow(10, 5 - sig);
  int i = round(f * mul);
  bool point = false;
  if (i == 1000000) { i = 100000; sig++; }
  if (sig < 0) {
    pfun('0'); pfun('.'); point = true;
    for (int j=0; j < - sig - 1; j++) pfun('0');
  }
  mul = 100000;
  for (int j=0; j<7; j++) {
    int d = (int)(i / mul);
    pfun(d + '0');
    i = i - d * mul;
    if (i == 0) {
      if (!point) {
        for (int k=j; k<sig; k++) pfun('0');
        pfun('.'); pfun('0');
      }
      return;
    }
    if (j == sig && sig >= 0) { pfun('.'); point = true; }
    mul = mul / 10;
  }
}

void pfloat (float f, pfun_t pfun) {
  if (isnan(f)) { pfstring(PSTR("NaN"), pfun); return; }
  if (f == 0.0) { pfun('0'); return; }
  if (isinf(f)) { pfstring(PSTR("Inf"), pfun); return; }
  if (f < 0) { pfun('-'); f = -f; }
  // Calculate exponent
  int e = 0;
  if (f < 1e-3 || f >= 1e5) {
    e = floor(log(f) / 2.302585); // log10 gives wrong result
    f = f / pow(10, e);
  }

  pmantissa (f, pfun);

  // Exponent
  if (e != 0) {
    pfun('e');
    pint(e, pfun);
  }
}

inline void pln (pfun_t pfun) {
  pfun('\n');
}

void pfl (pfun_t pfun) {
  if (LastPrint != '\n') pfun('\n');
}

void plist (object *form, pfun_t pfun) {
  pfun('(');
  printobject(car(form), pfun);
  form = cdr(form);
  while (form != NULL && listp(form)) {
    pfun(' ');
    printobject(car(form), pfun);
    form = cdr(form);
  }
  if (form != NULL) {
    pfstring(PSTR(" . "), pfun);
    printobject(form, pfun);
  }
  pfun(')');
}

void pstream (object *form, pfun_t pfun) {
  pfun('<');
  pfstring(streamname[(form->integer)>>8], pfun);
  pfstring(PSTR("-stream "), pfun);
  pint(form->integer & 0xFF, pfun);
  pfun('>');
}

void printobject (object *form, pfun_t pfun) {
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && isbuiltin(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) plist(form, pfun);
  else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != sym(NOTHING)) printsymbol(form, pfun); }
  else if (characterp(form)) pcharacter(form->chars, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (arrayp(form)) printarray(form, pfun);
  else if (form->type == CODE) pfstring(PSTR("code"), pfun);
  else if (streamp(form)) pstream(form, pfun);
  else error2(NIL, PSTR("error in print"));
}

void prin1object (object *form, pfun_t pfun) {
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(form, pfun);
  Flags = temp;
}

// Read functions

int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = LispLibrary[GlobalStringIndex++];
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    push(line, GCStack);
    eval(line, env);
    pop(GCStack);
    line = read(glibrary);
  }
}

// For line editor
const int TerminalWidth = 80;
volatile int WritePtr = 0, ReadPtr = 0;
const int KybdBufSize = 333; // 42*8 - 3
char KybdBuf[KybdBufSize];
volatile uint8_t KybdAvailable = 0;

// Parenthesis highlighting
void esc (int p, char c) {
  Serial.write('\e'); Serial.write('[');
  Serial.write((char)('0'+ p/100));
  Serial.write((char)('0'+ (p/10) % 10));
  Serial.write((char)('0'+ p % 10));
  Serial.write(c);
}

void hilight (char c) {
  Serial.write('\e'); Serial.write('['); Serial.write(c); Serial.write('m');
}

void Highlight (int p, int wp, uint8_t invert) {
  wp = wp + 2; // Prompt
#if defined (printfreespace)
  int f = Freespace;
  while (f) { wp++; f=f/10; }
#endif
  int line = wp/TerminalWidth;
  int col = wp%TerminalWidth;
  int targetline = (wp - p)/TerminalWidth;
  int targetcol = (wp - p)%TerminalWidth;
  int up = line-targetline, left = col-targetcol;
  if (p) {
    if (up) esc(up, 'A');
    if (col > targetcol) esc(left, 'D'); else esc(-left, 'C');
    if (invert) hilight('7');
    Serial.write('('); Serial.write('\b');
    // Go back
    if (up) esc(up, 'B'); // Down
    if (col > targetcol) esc(left, 'C'); else esc(-left, 'D');
    Serial.write('\b'); Serial.write(')');
    if (invert) hilight('0');
  }
}

void processkey (char c) {
  if (c == 27) { setflag(ESCAPE); return; }    // Escape key
#if defined(vt100)
  static int parenthesis = 0, wp = 0;
  // Undo previous parenthesis highlight
  Highlight(parenthesis, wp, 0);
  parenthesis = 0;
#endif
  // Edit buffer
  if (c == '\n' || c == '\r') {
    pserial('\n');
    KybdAvailable = 1;
    ReadPtr = 0;
    return;
  }
  if (c == 8 || c == 0x7f) {     // Backspace key
    if (WritePtr > 0) {
      WritePtr--;
      Serial.write(8); Serial.write(' '); Serial.write(8);
      if (WritePtr) c = KybdBuf[WritePtr-1];
    }
  } else if (WritePtr < KybdBufSize) {
    KybdBuf[WritePtr++] = c;
    Serial.write(c);
  }
#if defined(vt100)
  // Do new parenthesis highlight
  if (c == ')') {
    int search = WritePtr-1, level = 0;
    while (search >= 0 && parenthesis == 0) {
      c = KybdBuf[search--];
      if (c == ')') level++;
      if (c == '(') {
        level--;
        if (level == 0) {parenthesis = WritePtr-search-1; wp = WritePtr; }
      }
    }
    Highlight(parenthesis, wp, 1);
  }
#endif
  return;
}

int gserial () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
#if defined(lineeditor)
  while (!KybdAvailable) {
    while (!Serial.available());
    char temp = Serial.read();
    processkey(temp);
  }
  if (ReadPtr != WritePtr) return KybdBuf[ReadPtr++];
  KybdAvailable = 0;
  WritePtr = 0;
  return '\n';
#else
  unsigned long start = millis();
  while (!Serial.available()) if (millis() - start > 1000) clrflag(NOECHO);
  char temp = Serial.read();
  if (temp != '\n' && !tstflag(NOECHO)) pserial(temp);
  return temp;
#endif
}

object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(issp(ch)) ch = gfun();

  if (ch == ';') {
    do { ch = gfun(); if (ch == ';' || ch == '(') setflag(NOECHO); }
    while(ch != '(');
  }
  if (ch == '\n') ch = gfun();
  if (ch == -1) return nil;
  if (ch == ')') return (object *)KET;
  if (ch == '(') return (object *)BRA;
  if (ch == '\'') return (object *)QUO;

  // Parse string
  if (ch == '"') return readstring('"', gfun, true);

  // Parse symbol, character, or number
  int index = 0, base = 10, sign = 1;
  char buffer[BUFFERSIZE];
  int bufmax = BUFFERSIZE-3; // Max index
  unsigned int result = 0;
  bool isfloat = false;
  float fresult = 0.0;

  if (ch == '+') {
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '-') {
    sign = -1;
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '.') {
    buffer[index++] = ch;
    ch = gfun();
    if (ch == ' ') return (object *)DOT;
    isfloat = true;
  }

  // Parse reader macros
  else if (ch == '#') {
    ch = gfun();
    char ch2 = ch & ~0x20; // force to upper case
    if (ch == '\\') { // Character
      base = 0; ch = gfun();
      if (issp(ch) || ch == ')' || ch == '(') return character(ch);
      else LastChar = ch;
    } else if (ch == '|') {
      do { while (gfun() != '|'); }
      while (gfun() != '#');
      return nextitem(gfun);
    } else if (ch2 == 'B') base = 2;
    else if (ch2 == 'O') base = 8;
    else if (ch2 == 'X') base = 16;
    else if (ch == '\'') return nextitem(gfun);
    else if (ch == '.') {
      setflag(NOESC);
      object *result = eval(read(gfun), NULL);
      clrflag(NOESC);
      return result;
    }
    else if (ch == '(') { LastChar = ch; return readarray(1, read(gfun)); }
    else if (ch == '*') return readbitarray(gfun);
    else if (ch >= '1' && ch <= '9' && (gfun() & ~0x20) == 'A') return readarray(ch - '0', read(gfun));
    else error2(NIL, PSTR("illegal character after #"));
    ch = gfun();
  }
  int valid; // 0=undecided, -1=invalid, +1=valid
  if (ch == '.') valid = 0; else if (digitvalue(ch)<base) valid = 1; else valid = -1;
  bool isexponent = false;
  int exponent = 0, esign = 1;
  buffer[2] = '\0'; buffer[3] = '\0'; buffer[4] = '\0'; buffer[5] = '\0'; // In case symbol is < 5 letters
  float divisor = 10.0;

  while(!issp(ch) && ch != ')' && ch != '(' && index < bufmax) {
    buffer[index++] = ch;
    if (base == 10 && ch == '.' && !isexponent) {
      isfloat = true;
      fresult = result;
    } else if (base == 10 && (ch == 'e' || ch == 'E')) {
      if (!isfloat) { isfloat = true; fresult = result; }
      isexponent = true;
      if (valid == 1) valid = 0; else valid = -1;
    } else if (isexponent && ch == '-') {
      esign = -esign;
    } else if (isexponent && ch == '+') {
    } else {
      int digit = digitvalue(ch);
      if (digitvalue(ch)<base && valid != -1) valid = 1; else valid = -1;
      if (isexponent) {
        exponent = exponent * 10 + digit;
      } else if (isfloat) {
        fresult = fresult + digit / divisor;
        divisor = divisor * 10.0;
      } else {
        result = result * base + digit;
      }
    }
    ch = gfun();
  }

  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;
  if (isfloat && valid == 1) return makefloat(fresult * sign * pow(10, exponent * esign));
  else if (valid == 1) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2))
      return makefloat((float)result*sign);
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    const char* p = ControlCodes; char c = 0;
    while (c < 33) {
      if (strcasecmp(buffer, p) == 0) return character(c);
      p = p + strlen(p) + 1; c++;
    }
    if (index == 3) return character((buffer[0]*10+buffer[1])*10+buffer[2]-5328);
    error2(NIL, PSTR("unknown character"));
  }

  builtin_t x = lookupbuiltin(buffer);
  if (x == NIL) return nil;
  if (x != ENDFUNCTIONS) return bsymbol(x);
  else if ((index <= 6) && valid40(buffer)) return intern(twist(pack40(buffer)));
  buffer[index+1] = '\0'; buffer[index+2] = '\0'; buffer[index+3] = '\0'; // For internlong
  return internlong(buffer);
}

object *readrest (gfun_t gfun) {
  object *item = nextitem(gfun);
  object *head = NULL;
  object *tail = NULL;

  while (item != (object *)KET) {
    if (item == (object *)BRA) {
      item = readrest(gfun);
    } else if (item == (object *)QUO) {
      item = cons(bsymbol(QUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)DOT) {
      tail->cdr = read(gfun);
      if (readrest(gfun) != NULL) error2(NIL, PSTR("malformed list"));
      return head;
    } else {
      object *cell = cons(item, NULL);
      if (head == NULL) head = cell;
      else tail->cdr = cell;
      tail = cell;
      item = nextitem(gfun);
    }
  }
  return head;
}

object *read (gfun_t gfun) {
  object *item = nextitem(gfun);
  if (item == (object *)KET) error2(NIL, PSTR("incomplete list"));
  if (item == (object *)BRA) return readrest(gfun);
  if (item == (object *)DOT) return read(gfun);
  if (item == (object *)QUO) return cons(bsymbol(QUOTE), cons(read(gfun), NULL));
  return item;
}

// Setup

void initgfx () {
#if defined(gfxsupport)
  pinMode(SHARP_VDD, OUTPUT);
  digitalWrite(SHARP_VDD, HIGH);

  // pln(pserial);
  // pfstring(PSTR("starting mySPI with SHARP_MISO = "), pserial);
  // pint(SHARP_MISO, pserial);
  // pfstring(PSTR(", SHARP_MOSI = "), pserial);
  // pint(SHARP_MOSI, pserial);
  // pfstring(PSTR(", SHARP_SCK = "), pserial);
  // pint(SHARP_SCK, pserial);
  // pfstring(PSTR(", SHARP_CS = "), pserial);
  // pint(SHARP_CS, pserial);
  // pfstring(PSTR(" (IOM "), pserial);
  // pint(spi_get_peripheral_name(SHARP_MOSI, SHARP_MISO, SHARP_SCK), pserial);
  // pfstring(PSTR(") "), pserial);
  // pln(pserial);
  // mySPI.begin();
  delay(100);
  tft.begin();
  tft.clearDisplay();  
  tft.drawPixel(155 + 0, 115 + 0, COLOR_BLACK);
  tft.drawPixel(155 + 4, 115 + 0, COLOR_BLACK);
  tft.drawPixel(155 + 0, 115 + 3, COLOR_BLACK);
  tft.drawPixel(155 + 1, 115 + 4, COLOR_BLACK);
  tft.drawPixel(155 + 2, 115 + 4, COLOR_BLACK);
  tft.drawPixel(155 + 3, 115 + 4, COLOR_BLACK);
  tft.drawPixel(155 + 4, 115 + 3, COLOR_BLACK);
  tft.refresh();
#endif
}

void initenv () {
  GlobalEnv = NULL;
  tee = bsymbol(TEE);
}

extern "C" void keyboard_isr()
{
  /*
  I thought I could tft.toggleVcom(); here but no;
  Error Status: 0x80010133 Code: 307 Module: 1
  Error Message: Mutex: 0x1003F138, Not allowed in ISR context
  Location: 0x2DCFF
  Error Value: 0x1003F138
  Current Thread: rtx_idle Id: 0x10042134 Entry: 0x2DD0D StackSize: 0x200 StackMem: 0x10042478 SP: 0x1005FE3C 
  For more info, visit: https://mbed.com/s/error?error=0x80010133&tgt=SFE_ARTEMIS_ATP
  */

  keyEventNum++;
  int i, j;

  boolean shift = (currentKeyState[5] != 0);
  boolean control = (currentKeyState[12] != 0);
  boolean meta = (currentKeyState[13] != 0);
  boolean super = ((currentKeyState[0]&1) != 0) | ((currentKeyState[7]&2) != 0);
  boolean hyper = (currentKeyState[0] & 1<<7) != 0;

  int modifiers = (shift << 8) | (control << 9) | (meta << 10) | (super << 11) | (hyper << 12);

  for (int i = 0; i < COLS; i++) {
    currentKeyState[i] = 0;
  }

  for (i = 0; i < ROWS ; i++) {
    int row = rows[i];
    am_hal_gpio_output_tristate_enable(row);
    for (j = 0; j < COLS ; j++) {
      int col = cols[j];

      if (reportKeyAgainIn[i][j]) reportKeyAgainIn[i][j]--;

      uint32_t keyState = 0;
      am_hal_gpio_state_read(col, AM_HAL_GPIO_INPUT_READ, &keyState); // Yes, do this three times.
      am_hal_gpio_state_read(col, AM_HAL_GPIO_INPUT_READ, &keyState); // Gives it some
      am_hal_gpio_state_read(col, AM_HAL_GPIO_INPUT_READ, &keyState); // time to settle
      am_hal_gpio_state_read(col, AM_HAL_GPIO_INPUT_READ, &keyState); // time to settle
      am_hal_gpio_state_read(col, AM_HAL_GPIO_INPUT_READ, &keyState); // time to settle
      currentKeyState[j] |= (!keyState) << i;

      if (keyState == reportedKeyState[j] >> i) {
        if (reportKeyAgainIn[i][j] == 0) {
          if (keyState == 1) {
            reportedKeyState[j] = reportedKeyState[j] & !(1 << i);
          } else if (keyState == 0) {
            reportedKeyState[j] = reportedKeyState[j] | (1 << i);
          }
          reportKeyAgainIn[i][j] = DEBOUNCE_CYCLES;
          if (keyEventWritePtr >= keyEventReadPtr + KEYEVENT_BUFFER_SIZE) return; // we will lose keyEvents ...
          keyEvents[keyEventWritePtr % KEYEVENT_BUFFER_SIZE] = modifiers | (keyState << 15) | (shift ? KeymapShifted[i * COLS + j] : Keymap[i * COLS + j]);
          keyEventWritePtr++;
        }
      }
    }
    am_hal_gpio_output_tristate_disable(row);
  }
}

void setupKeyboard() {
  for (int i = 0; i < ROWS ; i++) {
    pinMode(rows[i], OUTPUT);
    digitalWrite(rows[i], LOW);
    pinMode(rows[i], INPUT);
    gpio[rows[i]] = new mbed::DigitalInOut(pinNameByIndex(pinIndexByNumber(rows[i])));
    gpio[rows[i]]->input();
    am_hal_gpio_pinconfig(rows[i], g_AM_HAL_GPIO_OUTPUT);
    am_hal_gpio_pinconfig(rows[i], g_AM_HAL_GPIO_TRISTATE);
    am_hal_gpio_state_write(rows[i], AM_HAL_GPIO_OUTPUT_CLEAR);
    am_hal_gpio_state_write(rows[i], AM_HAL_GPIO_OUTPUT_TRISTATE_DISABLE);
  }

  for (int j = 0; j < COLS ; j++) {
    pinMode(cols[j], INPUT_PULLUP);
    //
    //gpio[cols[j]] = new mbed::DigitalInOut(pinNameByIndex(pinIndexByNumber(cols[j])));
    am_hal_gpio_pinconfig(cols[j], g_AM_HAL_GPIO_INPUT_PULLUP);
    // am_hal_gpio_fastgpio_enable(cols[j]);
  }
}

void setupISR()
{
  int timerNum = 2;
  //  Configure timer.
  //  Refer to Ambiq Micro Apollo3 Blue MCU datasheet section 13.2.2
  //  and am_hal_ctimer.c line 710 of 2210.
  am_hal_ctimer_config_single(timerNum, AM_HAL_CTIMER_TIMERA,
                              AM_HAL_CTIMER_LFRC_512HZ |
                              AM_HAL_CTIMER_FN_REPEAT |
                              AM_HAL_CTIMER_INT_ENABLE);

  //  32 Hz = 1 tick on and 1 tick off, so the interrupt will trigger at 16Hz
  am_hal_ctimer_period_set(timerNum, AM_HAL_CTIMER_TIMERA, 1, 1);

  am_hal_ctimer_start(timerNum, AM_HAL_CTIMER_TIMERA);

  NVIC_EnableIRQ(CTIMER_IRQn);

  am_hal_ctimer_int_clear(AM_HAL_CTIMER_INT_TIMERA2);
  am_hal_ctimer_int_enable(AM_HAL_CTIMER_INT_TIMERA2);
  am_hal_ctimer_int_register(AM_HAL_CTIMER_INT_TIMERA2, keyboard_isr);
}

void disableISR() {
  am_hal_ctimer_int_disable(AM_HAL_CTIMER_INT_TIMERA2);
}

void slowISR() { // This was an attempt to save power that will be revisited
   int timerNum = 2;
   am_hal_ctimer_period_set(timerNum, AM_HAL_CTIMER_TIMERA, 100, 100);
}

void unslowISR() {
   int timerNum = 2;
   am_hal_ctimer_period_set(timerNum, AM_HAL_CTIMER_TIMERA, 1, 1);
}

void enableISR() {
  am_hal_ctimer_int_enable(AM_HAL_CTIMER_INT_TIMERA2);
}

void initsd() {
  // Configure pin 3 as VDD switch for SD card.

am_hal_gpio_pincfg_t PadDefVDD =
{
  .uFuncSel = 3,                                          // set pin as GPIO
  .ePowerSw = AM_HAL_GPIO_PIN_POWERSW_VDD,                // power switch to 3v3
  .ePullup = AM_HAL_GPIO_PIN_PULLUP_NONE,                 // no pullup
  .eDriveStrength = AM_HAL_GPIO_PIN_DRIVESTRENGTH_2MA,    // weak
  .eGPOutcfg = AM_HAL_GPIO_PIN_OUTCFG_PUSHPULL,           // A push-pull GPIO has the ability to both source and sink current
  .eGPInput = AM_HAL_GPIO_PIN_INPUT_ENABLE,               // enable for input
  .eIntDir = 0x0,                                         // NO interrupts
  .eGPRdZero = AM_HAL_GPIO_PIN_RDZERO_READPIN             // when read.. read as zeros
};

  am_hal_gpio_pinconfig(3, PadDefVDD);
  am_hal_gpio_state_write(3, AM_HAL_GPIO_OUTPUT_CLEAR);

  am_hal_gpio_pincfg_t PadDefVSS =
{
  .uFuncSel = 3,                                          // set pin as GPIO
  .ePowerSw = AM_HAL_GPIO_PIN_POWERSW_VSS,                // power switch GND
  .ePullup = AM_HAL_GPIO_PIN_PULLUP_NONE,                 // no pullup
  .eDriveStrength = AM_HAL_GPIO_PIN_DRIVESTRENGTH_2MA,    // weak
  .eGPOutcfg = AM_HAL_GPIO_PIN_OUTCFG_PUSHPULL,           // A push-pull GPIO has the ability to both source and sink current
  .eGPInput = AM_HAL_GPIO_PIN_INPUT_ENABLE,               // enable for input
  .eIntDir = 0x0,                                         // NO interrupts
  .eGPRdZero = AM_HAL_GPIO_PIN_RDZERO_READPIN             // when read.. read as zeros
};

  am_hal_gpio_pinconfig(37, PadDefVSS);
  am_hal_gpio_state_write(37, AM_HAL_GPIO_OUTPUT_CLEAR);
}

void sd_on() {
  initsd();
  am_hal_gpio_state_write(3, AM_HAL_GPIO_OUTPUT_SET);
  am_hal_gpio_state_write(37, AM_HAL_GPIO_OUTPUT_CLEAR);
  delay(100); // give the SD card time to initialize (TODO: check spec)
}

void sd_off() {
  delay(100); // give the SD card time to do any clean-up (TODO: check spec)
  am_hal_gpio_state_write(3, AM_HAL_GPIO_OUTPUT_CLEAR);
  am_hal_gpio_pinconfig(3, g_AM_HAL_GPIO_DISABLE);
  am_hal_gpio_pinconfig(5, g_AM_HAL_GPIO_DISABLE);
  am_hal_gpio_pinconfig(6, g_AM_HAL_GPIO_DISABLE);
  am_hal_gpio_pinconfig(7, g_AM_HAL_GPIO_DISABLE);
  am_hal_gpio_pinconfig(13, g_AM_HAL_GPIO_DISABLE);
  am_hal_gpio_pinconfig(37, g_AM_HAL_GPIO_DISABLE);
}

void setup () {
  // enableBurstMode();
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  initsd();
  setupKeyboard();
  setupISR();
  pln(pserial); pfstring(PSTR("--- uLisp 4.3a ---"), pserial); pln(pserial);
}

void killSerial() {
    Serial.end();
    // Don't backpower the serial chip
    am_hal_gpio_pinconfig(48, g_AM_HAL_GPIO_INPUT);
    am_hal_gpio_pinconfig(49, g_AM_HAL_GPIO_INPUT);
}

// Read/Evaluate/Print loop

void repl (object *env) {
  for (;;) {
    randomSeed(micros());
    gc(NULL, env);
    #if defined (printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pserial('>'); pserial(' ');

    object *line = read(gserial);

    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(NIL, PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);

    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}

void loop () {
  End = 0xA5;      // Canary to check stack
  if (!setjmp(toplevel_handler)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  // Come here after error
  delay(100); while (Serial.available()) Serial.read();
  clrflag(NOESC); BreakLevel = 0;
  for (int i=0; i<TRACEMAX; i++) TraceDepth[i] = 0;
  #if defined(sdcardsupport)
  SDpfile.close(); SDgfile.close();
  #endif
  #if defined(lisplibrary)
  killSerial(); // andreer: this reduces power use when unplugged - stops from backpowering serial chip. Comment this out to use serial.
  if (!tstflag(LIBRARYLOADED)) { setflag(LIBRARYLOADED); loadfromlibrary(NULL); }
  #endif
  #if defined(ULISP_WIFI)
  client.stop();
  #endif
  repl(NULL);
}
