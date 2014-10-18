


rdp (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  do {
     declaration (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        }
     if (state == NOMATCH) {
        production (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case NOMATCH: {
              if (symbol != EOF)
                 errmsg ("production expected", & state);
              while (symbol != EOF && symbol != ';')
                 getsym();     /* skip middle of prod */
              if (symbol == ';')
                 getsym();     /* advance to next prod */
              break;
              }
           }
        }
     } while (state == ACCEPT);
  switch (state) {
     case NOMATCH: {
        state = ACCEPT;
        break;
        }
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     if (symbol == EOF) {
        state = ACCEPT;
        }
     else {
        errmsg ("EOF expected", & state);
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  *gpst = state;
}



declaration (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  state = NOMATCH;
  if (symbol == '.') {
     state = ACCEPT;
     getsym ();
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     switch (symbol) {
        case TERMIDSYM: {
           state = ACCEPT;
           decl_tail (& state);
           break;
           }
        case NONTERMIDSYM: {
           state = ACCEPT;
           decl_tail (& state);
           break;
           }
        case EPSILONSYM: {
           state = ACCEPT;
           decl_tail (& state);
           break;
           }
        }
     if (state == NOMATCH) {
        errmsg ("missing declarator", & state);
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     state = NOMATCH;
     if (symbol == ';') {
        state = ACCEPT;
        getsym ();
        }
     else {
        errmsg ("missing semicolon", & state);
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  *gpst = state;
}



termlist (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  int i;
  do {
     state = NOMATCH;
     switch (symbol) {
        case NONTERMIDSYM: {
           state = ACCEPT;
           o_defn (symboltext, next_term_val);
           last_term_val = next_term_val;
           next_term_val++;
           terminal.key = malloc ((unsigned) strlen (symboltext)+1);
           strcpy (terminal.key, symboltext);
           terminal.data = (char *) NONTERMIDSYM;
           hsearch (terminal, ENTER);
           getsym ();
           break;
           }
        case INTSYM: {
           state = ACCEPT;
           next_term_val = atoi (symboltext);
           last_term_val = next_term_val;
           getsym ();
           break;
           }
        case '=': {
           state = ACCEPT;
           next_term_val = last_term_val;
           getsym ();
           break;
           }
        }
     } while (state == ACCEPT);
  switch (state) {
     case NOMATCH: {
        state = ACCEPT;
        break;
        }
     }
  *gpst = state;
}



extlist (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  do {
     state = NOMATCH;
     if (symbol == NONTERMIDSYM) {
        state = ACCEPT;
        terminal.key = malloc ((unsigned) strlen (symboltext) + 1);
        strcpy (terminal.key, symboltext);
        terminal.data = (char *) NONTERMIDSYM;
        hsearch (terminal, ENTER);
        getsym ();
        }
     } while (state == ACCEPT);
  switch (state) {
     case NOMATCH: {
        state = ACCEPT;
        break;
        }
     }
  *gpst = state;
}



production (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  char name [MAXLINE];
  state = NOMATCH;
  if (symbol == NONTERMIDSYM) {
     state = ACCEPT;
     strcpy (name, symboltext);
     getsym ();
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     if (symbol == ISSYM) {
        state = ACCEPT;
        getsym ();
        }
     else {
        errmsg ("missing '->'", & state);
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     actions (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case ACCEPT: {
           o_begin_routine (name);
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     rhs (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case NOMATCH: {
           errmsg ("missing right-hand-side", & state);
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     state = NOMATCH;
     if (symbol == ';') {
        state = ACCEPT;
        getsym ();
        }
     else {
        errmsg ("missing semicolon", & state);
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     actions (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case ACCEPT: {
           o_accept_actions();
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  if (state == ACCEPT)
     o_end_routine();
  *gpst = state;
}



rhs (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  int numalts;
  numalts = 0;
  actions (& state);
  switch (state) {
     case FAILURE: {
        *gpst = FAILURE;
        return;
        }
     case ACCEPT: {
        o_accept_actions();
        break;
        }
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     if (symbol == '$') {
        state = ACCEPT;
        o_selection_start();
        getsym ();
        }
     if (state == ACCEPT) {
        actions (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case ACCEPT: {
              if (num_actions > 0 || num_erractions > 0)
                 fprintf (stderr,
                  "%d:  actions are illegal here\n",
                  linenumber);
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        choice (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case NOMATCH: {
              errmsg ("missing choice", & state);
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        do {
           state = NOMATCH;
           if (symbol == '|') {
              state = ACCEPT;
              getsym ();
              }
           if (state == ACCEPT) {
              choice (& state);
              switch (state) {
                 case FAILURE: {
                    *gpst = FAILURE;
                    return;
                    }
                 case NOMATCH: {
                    errmsg ("missing choice", & state);
                    break;
                    }
                 }
              if (state != ACCEPT) {
                 *gpst = FAILURE;
                 return;
                 }
              }
           } while (state == ACCEPT);
        switch (state) {
           case NOMATCH: {
              state = ACCEPT;
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        state = ACCEPT;
        o_selection_end();
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        }
     if (state == NOMATCH) {
        alternative (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           }
        if (state == ACCEPT) {
           do {
              state = NOMATCH;
              if (symbol == '|') {
                 state = ACCEPT;
                 o_alt();
                 numalts++;
                 getsym ();
                 }
              if (state == ACCEPT) {
                 alternative (& state);
                 switch (state) {
                    case FAILURE: {
                       *gpst = FAILURE;
                       return;
                       }
                    case NOMATCH: {
                       errmsg ("missing alternative", & state);
                       break;
                       }
                    }
                 if (state != ACCEPT) {
                    *gpst = FAILURE;
                    return;
                    }
                 }
              } while (state == ACCEPT);
           switch (state) {
              case NOMATCH: {
                 state = ACCEPT;
                 for (; numalts > 0; numalts--)
                    o_endalt();
                 break;
                 }
              case ACCEPT: {
                 for (; numalts > 0; numalts--)
                    o_endalt();
                 break;
                 }
              }
           if (state != ACCEPT) {
              *gpst = FAILURE;
              return;
              }
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  *gpst = state;
}



choice (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  int more_than_one;
  state = NOMATCH;
  if (symbol == TERMIDSYM) {
     state = ACCEPT;
     o_choice_start (symboltext);
     getsym ();
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     if (symbol == '.') {
        state = ACCEPT;
        advance = NO;
        getsym ();
        }
     else {
        advance = YES;
        }
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     actions (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case ACCEPT: {
           o_choice_actions();
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     eltpresent (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case NOMATCH: {
           more_than_one = FALSE;
           break;
           }
        case ACCEPT: {
           o_begin_seq();
           more_than_one = TRUE;
           break;
           }
        }
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     do {
        term (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case ACCEPT: {
              o_test_seq_failure();
              break;
              }
           }
        if (state == NOMATCH) {
           nonterm (& state);
           switch (state) {
              case FAILURE: {
                 *gpst = FAILURE;
                 return;
                 }
              case ACCEPT: {
                 o_test_seq_failure();
                 break;
                 }
              }
           }
        } while (state == ACCEPT);
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  if (more_than_one)
     o_end_seq();
  o_choice_end();
  *gpst = state;
}



alternative (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  int more_than_one;
  term (& state);
  switch (state) {
     case FAILURE: {
        *gpst = FAILURE;
        return;
        }
     }
  if (state == NOMATCH) {
     nonterm (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case NOMATCH: {
           errmsg ("illegal term/nonterm", & state);
           break;
           }
        }
     }
  if (state == ACCEPT) {
     eltpresent (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case NOMATCH: {
           more_than_one = FALSE;
           break;
           }
        case ACCEPT: {
           o_begin_seq();
           more_than_one = TRUE;
           break;
           }
        }
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     do {
        term (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case ACCEPT: {
              o_test_seq_failure();
              break;
              }
           }
        if (state == NOMATCH) {
           nonterm (& state);
           switch (state) {
              case FAILURE: {
                 *gpst = FAILURE;
                 return;
                 }
              case ACCEPT: {
                 o_test_seq_failure();
                 break;
                 }
              }
           }
        } while (state == ACCEPT);
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  if (more_than_one)
     o_end_seq();
  *gpst = state;
}



eltpresent (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  state = NOMATCH;
  switch (symbol) {
     case NONTERMIDSYM: {
        state = ACCEPT;
        break;
        }
     case TERMIDSYM: {
        state = ACCEPT;
        break;
        }
     case EPSILONSYM: {
        state = ACCEPT;
        break;
        }
     case '(': {
        state = ACCEPT;
        break;
        }
     case '[': {
        state = ACCEPT;
        break;
        }
     case '{': {
        state = ACCEPT;
        break;
        }
     }
  *gpst = state;
}



term (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  char termbuf [MAXLINE];
  state = NOMATCH;
  if (symbol == TERMIDSYM) {
     state = ACCEPT;
     strcpy (termbuf, symboltext);
     getsym ();
     }
  if (state == ACCEPT) {
     state = NOMATCH;
     if (symbol == ':') {
        state = ACCEPT;
        getsym ();
        }
     else {
        o_match (termbuf);
        }
     if (state == ACCEPT) {
        state = NOMATCH;
        if (symbol == TERMIDSYM) {
           state = ACCEPT;
           o_match_range (termbuf, symboltext);
           getsym ();
           }
        else {
           errmsg ("missing upper bound", & state);
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        }
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     state = NOMATCH;
     if (symbol == '.') {
        state = ACCEPT;
        advance = NO;
        getsym ();
        }
     else {
        advance = YES;
        }
     switch (state) {
        case NOMATCH: {
           state = ACCEPT;
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     actions (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case ACCEPT: {
           o_end_term();
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  if (state == NOMATCH) {
     if (symbol == EPSILONSYM) {
        state = ACCEPT;
        o_epsilon();
        getsym ();
        }
     if (state == ACCEPT) {
        actions (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case ACCEPT: {
              o_accept_actions();
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        }
     }
  *gpst = state;
}



nonterm (gpst)
int *gpst;
{
  extern int symbol;
  int getsym();
  int state;
  state = NOMATCH;
  if (symbol == NONTERMIDSYM) {
     state = ACCEPT;
     o_call_nonterm (symboltext);
     getsym ();
     }
  if (state == ACCEPT) {
     actions (& state);
     switch (state) {
        case FAILURE: {
           *gpst = FAILURE;
           return;
           }
        case ACCEPT: {
           o_end_nonterm();
           break;
           }
        }
     if (state != ACCEPT) {
        *gpst = FAILURE;
        return;
        }
     }
  if (state == NOMATCH) {
     if (symbol == '(') {
        state = ACCEPT;
        getsym ();
        }
     if (state == ACCEPT) {
        rhs (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case NOMATCH: {
              errmsg ("missing rhs in parentheses", & state);
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        state = NOMATCH;
        if (symbol == ')') {
           state = ACCEPT;
           getsym ();
           }
        else {
           errmsg ("missing right parenthesis", & state);
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        actions (& state);
        switch (state) {
           case FAILURE: {
              *gpst = FAILURE;
              return;
              }
           case ACCEPT: {
              o_end_par();
              break;
              }
           }
        if (state != ACCEPT) {
           *gpst = FAILURE;
           return;
           }
        }
     if (state == NOMATCH) {
        if (symbol == '[') {
           state = ACCEPT;
           getsym ();
           }
        if (state == ACCEPT) {
           rhs (& state);
           switch (state) {
              case FAILURE: {
                 *gpst = FAILURE;
                 return;
                 }
              case NOMATCH: {
                 errmsg ("missing optional rhs", & state);
                 break;
                 }
              }
           if (state != ACCEPT) {
              *gpst = FAILURE;
              return;
              }
           state = NOMATCH;
           if (symbol == ']') {
              state = ACCEPT;
              getsym ();
              }
           else {
              errmsg ("missing right bracket", & state);
              }
           if (state != ACCEPT) {
              *gpst = FAILURE;
              return;
              }
           actions (& state);
           switch (state) {
              case FAILURE: {
                 *gpst = FAILURE;
                 return;
                 }
              case ACCEPT: {
                 o_end_opt();
                 break;
                 }
              }
           if (state != ACCEPT) {
              *gpst = FAILURE;
              return;
              }
           }
        if (state == NOMATCH) {
           if (symbol == '{') {
              state = ACCEPT;
              o_begin_rept();
              getsym ();
              }
           if (state == ACCEPT) {
              rhs (& state);
              switch (state) {
                 case FAILURE: {
                    *gpst = FAILURE;
                    return;
                    }
                 case NOMATCH: {
                    errmsg ("missing repeated rhs", & state);
                    break;
                    }
                 }
              if (state != ACCEPT) {
                 *gpst = FAILURE;
                 return;
                 }
              state = NOMATCH;
              if (symbol == '}') {
                 state = ACCEPT;
                 getsym ();
                 }
              else {
                 errmsg ("missing right brace", & state);
                 }
              if (state != ACCEPT) {
                 *gpst = FAILURE;
                 return;
                 }
              actions (& state);
              switch (state) {
                 case FAILURE: {
                    *gpst = FAILURE;
                    return;
                    }
                 case ACCEPT: {
                    o_end_rept();
                    break;
                    }
                 }
              if (state != ACCEPT) {
                 *gpst = FAILURE;
                 return;
                 }
              }
           }
        }
     }
  *gpst = state;
}
