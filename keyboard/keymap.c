#include QMK_KEYBOARD_H
#include "pointing_device.h"

enum layers { BASE, NOM, ARR, NAV, MOU, FUN, NUM, SYM, PLVR };

#define U_RDO SCMD(KC_Z)
#define U_PST LCMD(KC_V)
#define U_CPY LCMD(KC_C)
#define U_CUT LCMD(KC_X)
#define U_UND LCMD(KC_Z)
#define U_PSTFM LCMD(LALT(LSFT(KC_V)))

enum planck_keycodes {
  PLOVER = SAFE_RANGE,
  EXT_PLV,
  CMD_TAB,
  MY_CAPS,
  MS_RST,
  MS_LEFT,
  MS_RGHT,
  MS_UP,
  MS_DOWN,
  MS_UPLF,
  MS_UPRT,
  MS_DWLF,
  MS_DWRT
};

// use an unused keycode because of mod-tap keycode restrictions:
// https://docs.qmk.fm/#/mod_tap?id=caveats
#define S_ABSPC SFT_T(KC_PRIOR)

#define OS_ALT OSM(MOD_LALT)
#define OS_SFT OSM(MOD_LSFT)
#define CT LCTL_T
#define GT LGUI_T

#define SFT_ESC SFT_T(KC_ESC)
#define NAV_BSPC LT(NAV, KC_BSPC)
#define NUM_TAB LT(NUM, KC_TAB)
#define SYM_ENT LT(SYM, KC_ENT)

#define ALT_LEFT LALT(KC_LEFT)
#define ALT_RIGHT LALT(KC_RIGHT)

#define TAB_BAC C(S(KC_TAB))
#define TAB_FWD C(KC_TAB)

#define SCRCLIP G(C(S(KC_4)))
#define SCRSAVE G(S(KC_4))

#define INSP_EL G(A(KC_I))
#define SELC_EL G(S(KC_C))


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[BASE] = LAYOUT_ortho_4x12(
  KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    OS_ALT,  OS_ALT,  KC_J,    KC_L,    KC_U,    KC_Y,      KC_QUOT,
  KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    OS_ALT,  OS_ALT,  KC_M,    KC_N,    KC_E,    KC_I,      KC_O,
  GT(KC_Z),CT(KC_X),KC_C,    KC_D,    KC_V,    MO(MOU), MO(MOU), KC_K,    KC_H,    KC_COMM, CT(KC_DOT),GT(KC_SLSH),
  CMD_TAB, OS_ALT,  SFT_ESC, NAV_BSPC,NUM_TAB, OS_SFT,  OS_SFT,  SYM_ENT, KC_SPC,  S_ABSPC, XXXXXXX,   MO(FUN)
),
[NOM] = LAYOUT_ortho_4x12(
  KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    XXXXXXX, XXXXXXX, KC_J,    KC_L,    KC_U,    KC_Y,    KC_QUOT,
  KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    XXXXXXX, XXXXXXX, KC_M,    KC_N,    KC_E,    KC_I,    KC_O,
  KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    XXXXXXX, XXXXXXX, KC_K,    KC_H,    KC_COMM, KC_DOT,  KC_SLSH,
  XXXXXXX, XXXXXXX, KC_ESC,  KC_BSPC, KC_TAB,  XXXXXXX, XXXXXXX, KC_ENT,  KC_SPC,  KC_DEL,  DF(BASE),XXXXXXX
),
[ARR] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, KC_UP,   XXXXXXX, XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,
  XXXXXXX, KC_LEFT, KC_DOWN, KC_RGHT, XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, G(KC_GRV),XXXXXXX,XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,
  XXXXXXX, XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX, DF(BASE),XXXXXXX
),
[NAV] = LAYOUT_ortho_4x12(
  U_UND,   U_CUT,   U_CPY,   U_PST,   U_RDO,    XXXXXXX,XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,
  TAB_BAC, XXXXXXX, XXXXXXX, TAB_FWD, U_PSTFM,  XXXXXXX,XXXXXXX, MY_CAPS, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT,
  KC_LGUI, KC_LCTL, KC_LALT, KC_LSFT, G(KC_GRV),XXXXXXX,XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,  XXXXXXX,XXXXXXX, KC_ENT,  KC_SPC,  KC_LSFT, XXXXXXX, XXXXXXX
),
[MOU] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, MS_UPLF, MS_UP,   MS_UPRT, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, MS_LEFT, KC_BTN1, MS_RGHT, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, MS_DWLF, MS_DOWN, MS_DWRT, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, MS_RST,  XXXXXXX, XXXXXXX, XXXXXXX
),
[FUN] = LAYOUT_ortho_4x12(
  KC_F12,  KC_F7,   KC_F8,   KC_F9,   XXXXXXX, XXXXXXX, DM_REC2, DM_REC1, DM_RSTP, XXXXXXX, PLOVER,  RESET,
  KC_F11,  KC_F4,   KC_F5,   KC_F6,   KC_BRMU, XXXXXXX, DM_PLY2, DM_PLY1, SELC_EL, INSP_EL, DF(NOM), DF(ARR),
  KC_F10,  KC_F1,   KC_F2,   KC_F3,   KC_BRMD, XXXXXXX, XXXXXXX, XXXXXXX, KC_MUTE, KC_VOLD, KC_VOLU, XXXXXXX,
  XXXXXXX, XXXXXXX, KC_LSFT, KC_DEL,  KC_TAB,  XXXXXXX, XXXXXXX, SCRSAVE, SCRCLIP, XXXXXXX, XXXXXXX, XXXXXXX
),
[NUM] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_LBRC, KC_7,    KC_8,    KC_9,    KC_RBRC,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_EQL,  KC_4,    KC_5,    KC_6,    KC_COLN,
  KC_LGUI, KC_LCTL, KC_LALT, KC_LSFT, XXXXXXX, XXXXXXX, XXXXXXX, KC_BSLS, KC_1,    KC_2,    KC_3,    KC_GRV,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_MINS, KC_0,    KC_DOT,  XXXXXXX, XXXXXXX
),
[SYM] = LAYOUT_ortho_4x12(
  KC_LCBR, KC_AMPR, KC_ASTR, KC_LPRN, KC_RCBR, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  KC_SCLN, KC_DLR,  KC_PERC, KC_CIRC, KC_PLUS, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  KC_TILD, KC_EXLM, KC_AT,   KC_HASH, KC_PIPE, XXXXXXX, XXXXXXX, XXXXXXX, KC_LSFT, KC_LALT, KC_LCTL, KC_LGUI,
  XXXXXXX, XXXXXXX, KC_LSFT, KC_RPRN, KC_UNDS, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
),
[PLVR] = LAYOUT_ortho_4x12(
  XXXXXXX, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
  XXXXXXX, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, EXT_PLV, KC_1,    KC_C,    KC_V,    KC_N,    KC_M,    KC_1,    XXXXXXX, EXT_PLV, XXXXXXX
)

};

/**
 * "cmd-tab" key, as adapted from "super alt tab":
 *    https://docs.qmk.fm/#/feature_macros?id=super-alt↯tab
 *
 * When the key is tapped, a single cmd-tab key is tapped, switching to the
 * most recently used application (timeout set by TAPPING_TERM). When the key
 * is held, it gives the user a set time to view the applications (as
 * determined by CMD_TAB_BEFORE_FIRST_TIMEOUT). The is_cmd_tab_hold bool is set
 * during the first hold and the is_cmd_tab_before_first is set after the hold
 * but before the first tap after the hold. Once a key is tapped,
 * is_cmd_tab_active will be set. Each key tap will go to the next application,
 * and after the timeout (CMD_TAB_TIMEOUT) it will stop at the current
 * application.
 */
bool is_cmd_tab_active = false;
uint16_t cmd_tab_timer = 0;
#define CMD_TAB_TIMEOUT 500
bool is_cmd_tab_hold = false;
uint16_t cmd_tab_hold_timer = 0;
bool is_cmd_tab_before_first = false;
uint16_t cmd_tab_before_first_timer = 0;
#define CMD_TAB_BEFORE_FIRST_TIMEOUT 3000

static const int16_t WINDOW_WIDTH = 2560;
static const int16_t WINDOW_HEIGHT = 1440;
// binary search boundary for custom mouse keys
int16_t ms_bs_left = 0;
int16_t ms_bs_right = WINDOW_WIDTH;
int16_t ms_bs_up = 0;
int16_t ms_bs_down = WINDOW_HEIGHT;

// three piecewise linear functions are needed to convert pixel values to mouse
// input values
// https://www.desmos.com/calculator/mms2oq4qrf
static const double scale = 1.00;
static const double a1 = scale * 1;
static const double b1 = scale * 2.1;
static const double a2 = scale * 0.413;
static const double b2 = scale * 15.5;
static const double a3 = scale * 1.02;
static const double b3 = scale * -66.9;
static const double d1 = scale * 22.9;
static const double d2 = scale * 136.6;
static const double d3 = scale * 190;
// max travel distance (pixels) corresponds to the max raw (mouse input) value
static const int16_t MAX_TRAVEL_DISTANCE = d3;
static const int8_t MAX_RAW_DISTANCE = 127;

// convert screen pixels to mouse input values
int8_t convert_coord(int16_t dist) {
  if (dist == 0) return 0;
  if (dist < 0) return -convert_coord(-dist);

  if (dist < d1) return a1 * dist + b1;
  if (dist < d2) return a2 * dist + b2;
  if (dist < d3 || dist == MAX_TRAVEL_DISTANCE) return a3 * dist + b3;

  // too high
  return 0;
}

report_mouse_t currentReport;
static const int8_t EPSILON = 2;
// send raw mouse movements
void move_mouse_raw(int8_t x, int8_t y) {
  currentReport = pointing_device_get_report();
  currentReport.x = x;
  currentReport.y = y;
  host_mouse_send(&currentReport);
}

// move the mouse given screen pixels
// will first move in the horizontal direction, then the vertical direction
// automatically converts pixels to mouse input units
void move_mouse_delta(int16_t x, int16_t y) {
  int8_t x_sign = x < 0 ? -1 : 1;
  int8_t y_sign = y < 0 ? -1 : 1;

  for (int16_t cur_x = abs(x); cur_x > 0; cur_x -= MAX_TRAVEL_DISTANCE) {
    if (cur_x < MAX_TRAVEL_DISTANCE) {
      move_mouse_raw(x_sign * convert_coord(cur_x), 0);
    } else {
      move_mouse_raw(x_sign * MAX_RAW_DISTANCE, 0);
    }
  }

  for (int16_t cur_y = abs(y); cur_y > 0; cur_y -= MAX_TRAVEL_DISTANCE) {
    if (cur_y < MAX_TRAVEL_DISTANCE) {
      move_mouse_raw(0, y_sign * convert_coord(cur_y));
    } else {
      move_mouse_raw(0, y_sign * MAX_RAW_DISTANCE);
    }
  }
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  // check for mod-tap alt-backspace separately to optimize the switch statement
  if (keycode == S_ABSPC) {
    if (!record->event.pressed && record->tap.count > 0) {
      SEND_STRING(SS_LALT(SS_TAP(X_BSPC)));
    }
    return true;
  }

  switch (keycode) {
    case PLOVER:
      if (record->event.pressed) {
        layer_move(PLVR);
        // start plover/plojo with ctrl-alt-shift-p
        SEND_STRING(SS_LSFT(SS_TAP(X_F7)));
      }
      return false;
    case EXT_PLV:
      if (record->event.pressed) {
        layer_move(BASE);
        // exit plover/plojo with "PHR*OF" stroke (erfvyu keys)
        // all the keys must be pressed down at the same time to simulate a steno chord
        SEND_STRING(SS_DOWN(X_E) SS_DOWN(X_R) SS_DOWN(X_F) SS_DOWN(X_V) SS_DOWN(X_Y) SS_DOWN(X_U));
        SEND_STRING(SS_UP(X_E) SS_UP(X_R) SS_UP(X_F) SS_UP(X_V) SS_UP(X_Y) SS_UP(X_U));
      }
      return false;
    case CMD_TAB:
      if (record->event.pressed) {
        if (!is_cmd_tab_active) {
          is_cmd_tab_hold = true;
          cmd_tab_hold_timer = timer_read();
        } else {
          is_cmd_tab_before_first = false;
          cmd_tab_timer = timer_read();
          register_code(KC_TAB);
        }
      } else {
        if (is_cmd_tab_hold) {
          if (timer_elapsed(cmd_tab_hold_timer) < TAPPING_TERM) {
            register_code(KC_LGUI);
            tap_code(KC_TAB);
            unregister_code(KC_LGUI);
            is_cmd_tab_hold = false;
          }
        }
        if (is_cmd_tab_active) {
          unregister_code(KC_TAB);
        }
      }
      return false;
    case MY_CAPS:
      if (record->event.pressed) {
        tap_code_delay(KC_CAPS, 200);
      }
      return false;
    case MS_RST:
      if (record->event.pressed) {
        // move mouse to upper-left corner to calibrate position
        for (uint8_t i = 0; i < 20; i++) {
          move_mouse_raw(-MAX_RAW_DISTANCE, -MAX_RAW_DISTANCE);
        }
        // initialize mouse binary search
        ms_bs_left = 0;
        ms_bs_right = WINDOW_WIDTH;
        ms_bs_up = 0;
        ms_bs_down = WINDOW_HEIGHT;
        // start binary search at the middle
        int16_t x_middle = (ms_bs_left + ms_bs_right) / 2;
        int16_t y_middle = (ms_bs_up + ms_bs_down) / 2;
        move_mouse_delta(x_middle, y_middle);
      }
      return false;
    case MS_LEFT:
      if (record->event.pressed) {
        int16_t old_middle = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle = (ms_bs_left + old_middle) / 2;
        move_mouse_delta(new_middle - old_middle, 0);
        ms_bs_right = old_middle;
      }
      return false;
    case MS_RGHT:
      if (record->event.pressed) {
        int16_t old_middle = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle = (old_middle + ms_bs_right) / 2;
        move_mouse_delta(new_middle - old_middle, 0);
        ms_bs_left = old_middle;
      }
      return false;
    case MS_UP:
      if (record->event.pressed) {
        int16_t old_middle = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle = (ms_bs_up + old_middle) / 2;
        move_mouse_delta(0, new_middle - old_middle);
        ms_bs_down = old_middle;
      }
      return false;
    case MS_DOWN:
      if (record->event.pressed) {
        int16_t old_middle = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle = (old_middle + ms_bs_down) / 2;
        move_mouse_delta(0, new_middle - old_middle);
        ms_bs_up = old_middle;
      }
      return false;
    case MS_UPLF:
      if (record->event.pressed) {
        int16_t old_middle_y = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle_y = (ms_bs_up + old_middle_y) / 2;
        int16_t old_middle_x = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle_x = (ms_bs_left + old_middle_x) / 2;
        move_mouse_delta(new_middle_x - old_middle_x, new_middle_y - old_middle_y);
        ms_bs_right = old_middle_x;
        ms_bs_down = old_middle_y;
      }
      return false;
    case MS_UPRT:
      if (record->event.pressed) {
        int16_t old_middle_y = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle_y = (ms_bs_up + old_middle_y) / 2;
        int16_t old_middle_x = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle_x = (old_middle_x + ms_bs_right) / 2;
        move_mouse_delta(new_middle_x - old_middle_x, new_middle_y - old_middle_y);
        ms_bs_left = old_middle_x;
        ms_bs_down = old_middle_y;
      }
      return false;
    case MS_DWLF:
      if (record->event.pressed) {
        int16_t old_middle_y = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle_y = (old_middle_y + ms_bs_down) / 2;
        int16_t old_middle_x = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle_x = (ms_bs_left + old_middle_x) / 2;
        move_mouse_delta(new_middle_x - old_middle_x, new_middle_y - old_middle_y);
        ms_bs_right = old_middle_x;
        ms_bs_up = old_middle_y;
      }
      return false;
    case MS_DWRT:
      if (record->event.pressed) {
        int16_t old_middle_y = (ms_bs_up + ms_bs_down) / 2;
        int16_t new_middle_y = (old_middle_y + ms_bs_down) / 2;
        int16_t old_middle_x = (ms_bs_left + ms_bs_right) / 2;
        int16_t new_middle_x = (old_middle_x + ms_bs_right) / 2;
        move_mouse_delta(new_middle_x - old_middle_x, new_middle_y - old_middle_y);
        ms_bs_left = old_middle_x;
        ms_bs_up = old_middle_y;
      }
      return false;
  }

  // reset cmd-tab if the cmd mod was released
  if ((keycode >> 8) & MOD_LGUI && !record->event.pressed) {
    is_cmd_tab_active = false;
    is_cmd_tab_before_first = false;
  }

  return true;
}

void matrix_scan_user(void) {
  if (is_cmd_tab_before_first) {
    if (timer_elapsed(cmd_tab_before_first_timer) > CMD_TAB_BEFORE_FIRST_TIMEOUT) {
      unregister_code(KC_LGUI);
      is_cmd_tab_before_first = false;
      is_cmd_tab_active = false;
    }
  } else {
    if (is_cmd_tab_active) {
      if (timer_elapsed(cmd_tab_timer) > CMD_TAB_TIMEOUT) {
        unregister_code(KC_LGUI);
        is_cmd_tab_active = false;
      }
    }
  }
  if (is_cmd_tab_hold) {
    if (timer_elapsed(cmd_tab_hold_timer) > TAPPING_TERM) {
      register_code(KC_LGUI);
      tap_code(KC_TAB);
      is_cmd_tab_hold = false;
      is_cmd_tab_active = true;
      is_cmd_tab_before_first = true;
      cmd_tab_before_first_timer = timer_read();
    }
  }
}
