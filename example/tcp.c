#include <stdio.h>
#include <stdint.h>

struct tcp_create {
  int state;
  uint8_t tcp_create_established;
  int tcp_process_return;
};


int tcp_create_event_Net_Tx_ACK(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 4:
      /* event S_seq_61 -> S_seq_62 */
      /* register tcp_process_return = 63 */
      /* event S_seq_61 -> S_initial_1 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = 63;
      s2[curpos].state = 26;
      curpos++;
      break;
    case 18:
      /* event S_seq_5 -> S_seq_6 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 19;
      curpos++;
      break;
    case 28:
      /* event S_seq_24 -> S_either_or_17 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 29;
      curpos++;
      break;
    case 30:
      /* event S_seq_19 -> S_seq_20 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 21;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Timeout_2MSL(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 29:
      /* event S_either_or_17 -> S_either_or_3 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 31;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Tx_FIN(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 20:
      /* event S_seq_7 -> S_or_10 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 22;
      curpos++;
      /* event S_seq_7 -> S_or_12 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 27;
      curpos++;
      break;
    case 24:
      /* event S_seq_15 -> S_or_18 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 17;
      curpos++;
      /* event S_seq_15 -> S_or_22 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 32;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Rx_FIN_ACK(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 21:
      /* event S_seq_20 -> S_either_or_17 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 29;
      curpos++;
      break;
    case 22:
      /* event S_or_10 -> S_either_or_3 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 31;
      curpos++;
      break;
    case 32:
      /* event S_or_22 -> S_seq_23 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 25;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Tx_SYN(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 12:
      /* event S_seq_50 -> S_or_55 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 10;
      curpos++;
      /* event S_seq_50 -> S_or_57 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 13;
      curpos++;
      /* event S_seq_50 -> S_or_60 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 2;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Timeout(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 10:
      /* event S_or_55 -> S_either_or_54 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 1;
      curpos++;
      break;
    case 27:
      /* event S_or_12 -> S_either_or_3 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 31;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Free_TCB(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 1:
      /* event S_either_or_54 -> S_final_31 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 7;
      curpos++;
      break;
    case 3:
      /* event S_seq_41 -> S_do_37 */
      if (! tcp_create_established) then begin
        /* event S_seq_41 -> S_seq_36 */
        s2[curpos].tcp_create_established = s1[i].tcp_create_established;
        s2[curpos].tcp_process_return = s1[i].tcp_process_return;
        s2[curpos].state = 11;
        curpos++;
      end;
      if tcp_create_established then begin
        /* event S_seq_41 -> S_final_31 */
        s2[curpos].tcp_create_established = s1[i].tcp_create_established;
        s2[curpos].tcp_process_return = s1[i].tcp_process_return;
        s2[curpos].state = 7;
        curpos++;
      end;
      break;
    case 31:
      /* event S_either_or_3 -> S_final_2 */
      switch (s1[curpos].tcp_process_return) {
       case 63:
        /* event S_either_or_3 -> S_final_31 */
        s2[curpos].tcp_create_established = s1[i].tcp_create_established;
        s2[curpos].tcp_process_return = s1[i].tcp_process_return;
        s2[curpos].state = 7;
        curpos++;
        break;
       case 45:
        /* event S_either_or_3 -> S_do_37 */
        if (! tcp_create_established) then begin
          /* event S_either_or_3 -> S_seq_36 */
          s2[curpos].tcp_create_established = s1[i].tcp_create_established;
          s2[curpos].tcp_process_return = s1[i].tcp_process_return;
          s2[curpos].state = 11;
          curpos++;
        end;
        if tcp_create_established then begin
          /* event S_either_or_3 -> S_final_31 */
          s2[curpos].tcp_create_established = s1[i].tcp_create_established;
          s2[curpos].tcp_process_return = s1[i].tcp_process_return;
          s2[curpos].state = 7;
          curpos++;
        end;
        break;
       default:
        tesla_internal_error ();
      }
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Rx_SYN_ACK(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 2:
      /* event S_or_60 -> S_seq_61 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 4;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_App_Connect(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 6:
      /* event S_initial_30 -> S_seq_50 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 12;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Rx_RST(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 16:
      /* event S_or_40 -> S_seq_41 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 3;
      curpos++;
      break;
    case 26:
      /* event S_initial_1 -> S_either_or_3 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 31;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Allocate_TCB(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 9:
      /* event S_seq_35 -> S_seq_36 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 11;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_App_Listen(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 6:
      /* event S_initial_30 -> S_seq_34 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 8;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Rx_FIN(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 17:
      /* event S_or_18 -> S_seq_19 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 30;
      curpos++;
      break;
    case 25:
      /* event S_seq_23 -> S_seq_24 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 28;
      curpos++;
      break;
    case 26:
      /* event S_initial_1 -> S_seq_5 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 18;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Tx_SYN_ACK(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 11:
      /* event S_seq_36 -> S_or_40 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 16;
      curpos++;
      /* event S_seq_36 -> S_or_43 */
      /* register tcp_create_established = 1 */
      /* event S_seq_36 -> S_assign_44 */
      /* register tcp_process_return = 45 */
      /* event S_seq_36 -> S_initial_1 */
      s2[curpos].tcp_create_established = 1;
      s2[curpos].tcp_process_return = 45;
      s2[curpos].state = 26;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_Net_Rx_SYN(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 8:
      /* event S_seq_34 -> S_seq_35 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 9;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}

int tcp_create_event_App_Close(struct tcp_create *s1, int sz, struct tcp_create *s2) {
  uint8_t curpos = 0;
  for (uint8_t i=0; i<sz; i++) {
    switch (s1[i].state) {
    case 13:
      /* event S_or_57 -> S_either_or_54 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 1;
      curpos++;
      break;
    case 19:
      /* event S_seq_6 -> S_seq_7 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 20;
      curpos++;
      break;
    case 26:
      /* event S_initial_1 -> S_seq_15 */
      s2[curpos].tcp_create_established = s1[i].tcp_create_established;
      s2[curpos].tcp_process_return = s1[i].tcp_process_return;
      s2[curpos].state = 24;
      curpos++;
      break;
    default:
      tesla_error();
    }
  }
}


