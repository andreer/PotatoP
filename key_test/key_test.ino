//
// Keyboard scanning test
//
#include <Adafruit_GFX.h>
#include <Adafruit_SharpMem.h>

// any pins can be used

#define SHARP_SCK  SPI_CLK // SPI_CLK
#define SHARP_MOSI SPI_SDO
#define SHARP_MISO NC
#define SHARP_CS   D13
#define SHARP_VDD  D12

// Set the size of the display here, e.g. 144x168!
Adafruit_SharpMem display(SHARP_SCK, SHARP_MOSI, SHARP_CS, 320, 240);
// The currently-available SHARP Memory Display (144x168 pixels)
// requires > 4K of microcontroller RAM; it WILL NOT WORK on Arduino Uno
// or other <4K "classic" devices!  The original display (96x96 pixels)
// does work there, but is no longer produced.

#define BLACK 0
#define WHITE 1

#define KEYEVENT_BUFFER_SIZE 1000
volatile uint32_t keyEventWritePtr = 0;
volatile uint32_t keyEventReadPtr = 0;
volatile uint32_t keyEvents[KEYEVENT_BUFFER_SIZE];
uint32_t keyEventNum = 0;

#define ROWS 8
#define COLS 14
#define DEBOUNCE_CYCLES 1 // doesn't really do much with how slow the scanning is ...

int rows[] = { 42, 43, 0, 1, 2, 45, 41, 17 };
int cols[] = { 18, 19, 15, 26, 9, 10, 8, 14, 35, 4, 22, 23, 27, 28 };

mbed::DigitalInOut* gpio[48];

const char Keymap[] PROGMEM =
  "@>mcz@\n@@<vx@@"//0
  "@ljda@\\@;kfs@@"//1
  "@@n@@@ @/@b@@@" //2
  "@@h@@@@@'@g@@@" //3
  "@@y@@@\b@[]t@@@" //4
  "@9731@@@@842@@" //5
  "@oueq@@@pirw@@" //6
  "@@6@`@@@-=5@@@";//7
// 01234567890abc

//&Keymap[(3-row)*11 + column + 44*shift]

uint8_t currentKeyState[COLS];
uint8_t reportedKeyState[COLS];
uint8_t reportKeyAgainIn[ROWS][COLS];

extern "C" void my_isr()
{
  keyEventNum++;
  int i, j;

  for (int i = 0; i < COLS; i++) {
    currentKeyState[i] = 0;
  }

  for (i = 0; i < ROWS ; i++) {
    int row = rows[i];
    //    pinMode(row, OUTPUT);
    gpio[row]->output();
    //    am_hal_gpio_state_write(row, AM_HAL_GPIO_OUTPUT_CLEAR);
    for (j = 0; j < COLS ; j++) {
      int col = cols[j];

      if (reportKeyAgainIn[i][j]) reportKeyAgainIn[i][j]--;

      int keyState = am_hal_gpio_input_read(col);
      //      int keyState = rand()%2;
      //      int keyState = digitalRead(col);
      //      int keyState = digitalRead(col);
      currentKeyState[j] |= (!keyState) << i;

      if (keyState == reportedKeyState[j] >> i) {
        //        Serial.println(reportKeyAgainIn[i][j]);
        if (reportKeyAgainIn[i][j] == 0) {
          //          Serial.println("here");
          if (keyState == 1) {
            reportedKeyState[j] = reportedKeyState[j] & !(1 << i);
          } else if (keyState == 0) {
            reportedKeyState[j] = reportedKeyState[j] | (1 << i);
          }
          reportKeyAgainIn[i][j] = DEBOUNCE_CYCLES;
          if (keyEventWritePtr >= keyEventReadPtr + KEYEVENT_BUFFER_SIZE) return; // we will lose keyEvents ...
          keyEvents[keyEventWritePtr % KEYEVENT_BUFFER_SIZE] = (keyState << 8) + Keymap[i * COLS + j];
          keyEventWritePtr++;
          //          Serial.printf("row %d, col %d is '", i, j);
          //          Serial.print(Keymap[i * COLS + j]); Serial.println("'");
        }
      }
    }
    //    pinMode(row, INPUT);
    gpio[row]->input();
    //    am_hal_gpio_state_write(row, AM_HAL_GPIO_OUTPUT_TRISTATE_DISABLE);
  }
  //
  //
  //  Serial.print("Current: ");
  //  for (int i = 0; i < COLS; i++) {
  //    Serial.print(currentKeyState[i]);
  //    Serial.print(" ");
  //  }
  //  Serial.println();
  //
  //  Serial.print("Reported: ");
  //  for (int i = 0; i < COLS; i++) {
  //    Serial.print(reportedKeyState[i]);
  //    Serial.print(" ");
  //  }
  //  Serial.println();
  //  Serial.println();
  //  Serial.println();
}

void setupKeyboard() {
  for (int i = 0; i < ROWS ; i++) {
    pinMode(rows[i], OUTPUT);
    digitalWrite(rows[i], LOW);
    pinMode(rows[i], INPUT);
    gpio[rows[i]] = new mbed::DigitalInOut(pinNameByIndex(pinIndexByNumber(rows[i])));
    gpio[rows[i]]->input();
    //am_hal_gpio_pinconfig(rows[i], g_AM_HAL_GPIO_OUTPUT);
  }

  for (int j = 0; j < COLS ; j++) {
    pinMode(cols[j], INPUT_PULLUP);
    //
    //gpio[cols[j]] = new mbed::DigitalInOut(pinNameByIndex(pinIndexByNumber(cols[j])));
    //am_hal_gpio_pinconfig(cols[j], g_AM_HAL_GPIO_INPUT_PULLUP_24);
    am_hal_gpio_fastgpio_enable(cols[j]);
  }
}

void setup( ) {
  Serial.begin(115200);
  while (!Serial) {
    ; // Wait for serial port to connect. Needed for native USB port only.
  }
  Serial.println("CTIMER will trigger an interrupt, calling the function my_isr at regular intervals.");

  setupKeyboard();

  Serial.println("Set up keyboard");

  pinMode(SHARP_CS, OUTPUT);
  digitalWrite(SHARP_CS, LOW);
  pinMode(SHARP_VDD, OUTPUT);
  digitalWrite(SHARP_VDD, HIGH);
  pinMode(SPI_CLK, OUTPUT);
  digitalWrite(SPI_CLK, LOW);

  display.begin();

  Serial.println("Display initialized");

  display.clearDisplay();

  int t0 = micros();
  for (int i = 0; i < KEYEVENT_BUFFER_SIZE; i++) {
    my_isr();
  }
  int t1 = micros();

  float ms = t1 - t0;
  Serial.print("Called "); Serial.print(KEYEVENT_BUFFER_SIZE); Serial.print(" times in "); Serial.print(ms); Serial.println(" microseconds");

  Serial.print(ms / KEYEVENT_BUFFER_SIZE); Serial.println(" microseconds per iteration ");

  //my_isr();

  display.fillCircle(display.width() - 25, display.height() - 25 , 24, BLACK);
  display.refresh();

  setupISR(); // timerNum, period, padNum
}

int iteration = 0;
int last = 0;
int lastcolor = BLACK;
int textposx = 1;
int textposy = 1;
int scale = 1;
void loop( ) {
  //  delay(100000);
  //Serial.print("loop iteration ");

  //Serial.print(iteration++);
  //Serial.print(", reading up to ");
  //Serial.println(readTo);

  display.fillCircle(display.width() - 25, display.height() - 25, 22, lastcolor);
  display.refresh();

  if (lastcolor == BLACK) {
    lastcolor = WHITE;
  } else {
    lastcolor = BLACK;
  }


  uint32_t readTo = keyEventWritePtr;
  while (keyEventReadPtr < readTo) {
    int k = keyEvents[keyEventReadPtr % KEYEVENT_BUFFER_SIZE];
    keyEventReadPtr++;

    if (k < 0xff) {
      Serial.print((char) k);


      if (k == '\n') {
        textposx = 1;
        textposy++;
      } else if (k == '\b') {
        display.drawChar(--textposx * (6 * scale), textposy * (8 * scale), ' ', BLACK, WHITE, scale);
      } else {
        display.drawChar(textposx++ * (6 * scale), textposy * (8 * scale), (char)k, BLACK, WHITE, scale);
      }
    }
    //Serial.print("Read key event ");
    //Serial.print(keyEvents[keyEventReadPtr % KEYEVENT_BUFFER_SIZE]);
    //Serial.print(" from index ");
    //Serial.println(keyEventReadPtr % KEYEVENT_BUFFER_SIZE);

  }

    int num = millis();
    Serial.println(num - last);
    last = num;
//    delay(1000);
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

  //  Set the timing parameters.
  //  32 Hz = 1 tick on and 1 tick off, so the interrupt will trigger at 16Hz
  am_hal_ctimer_period_set(timerNum, AM_HAL_CTIMER_TIMERA, 1, 1);

  am_hal_ctimer_start(timerNum, AM_HAL_CTIMER_TIMERA);

  NVIC_EnableIRQ(CTIMER_IRQn);

  Serial.println("CTIMER started");

  am_hal_ctimer_int_clear(AM_HAL_CTIMER_INT_TIMERA2);
  am_hal_ctimer_int_enable(AM_HAL_CTIMER_INT_TIMERA2);
  am_hal_ctimer_int_register(AM_HAL_CTIMER_INT_TIMERA2, my_isr);

  Serial.print("Timer A");
  Serial.print(timerNum);
  Serial.println(" configured");
}
