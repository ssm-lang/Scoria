#include "peng-platform.h"
#include <stdio.h>
typedef struct {
    /* Procedure generic fields */;
    void (*step)(rar_t *);
    uint16_t  pc;
    rar_t    *caller;
    uint16_t  children;
    uint32_t  priority;
    uint8_t   depth;
    bool      scheduled;
    /* Procedure specific fields */;
    cv_int_t *r // Procedure argument;
    trigger_t trig1;
} rar_mywait_t;

rar_mywait_t *enter_mywait(rar_t *caller, uint32_t priority, uint8_t depth, cv_int_t *r) {
    rar_mywait_t *rar = (rar_mywait_t *) enter(sizeof(rar_mywait_t), step_mywait, caller, priority, depth);
    rar->r = r;
    rar->trig1.rar = (rar_t *) rar;
};

void step_mywait(rar_t *gen_rar) {
    rar_mywait_t *rar = (rar_mywait_t *) gen_rar;
    switch(rar->pc) {
        case 0:
            sensitize((cv_t *) rar->r, &rar->trig1);
            rar->pc = 1;
            return;

        case 1:
            desensitize(&rar->trig1);

        leave((rar_t *) rar, sizeof(rar_mywait_t));
    }
}

typedef struct {
    /* Procedure generic fields */;
    void (*step)(rar_t *);
    uint16_t  pc;
    rar_t    *caller;
    uint16_t  children;
    uint32_t  priority;
    uint8_t   depth;
    bool      scheduled;
    /* Procedure specific fields */;
    cv_int_t *r1 // Procedure argument;
    cv_int_t *r2 // Procedure argument;
    cv_int_t *r // Procedure argument;
    cv_int_t v1 // Declared at line 16, column 5 in file Fib.hs;
    cv_int_t v2 // Declared at line 17, column 5 in file Fib.hs;
} rar_mysum_t;

rar_mysum_t *enter_mysum(rar_t *caller, uint32_t priority, uint8_t depth, cv_int_t *r1, cv_int_t *r2, cv_int_t *r) {
    rar_mysum_t *rar = (rar_mysum_t *) enter(sizeof(rar_mysum_t), step_mysum, caller, priority, depth);
    rar->r1 = r1;
    rar->r2 = r2;
    rar->r = r;
};

void step_mysum(rar_t *gen_rar) {
    rar_mysum_t *rar = (rar_mysum_t *) gen_rar;
    switch(rar->pc) {
        case 0:
            uint8_t new_depth = rar->depth - 1;
            uint32_t pinc = 1 << new_depth;
            uint32_t new_priority = rar->priority;
            fork((rar_t *) enter_mywait( (rar_t *) rar
                                       , new_priority
                                       , new_depth
                                       , rar->r1));
            new_priority += pinc;
            fork((rar_t *) enter_mywait( (rar_t *) rar
                                       , new_priority
                                       , new_depth
                                       , rar->r2));
            rar->pc = 1;
            return;

        case 1:
            assign_int(&rar->v1, rar->priority, rar->r1->value);
            assign_int(&rar->v2, rar->priority, rar->r2->value);
            later_int(rar->r, now + 1, (rar->v1.value) + (rar->v2.value));

        leave((rar_t *) rar, sizeof(rar_mysum_t));
    }
}

typedef struct {
    /* Procedure generic fields */;
    void (*step)(rar_t *);
    uint16_t  pc;
    rar_t    *caller;
    uint16_t  children;
    uint32_t  priority;
    uint8_t   depth;
    bool      scheduled;
    /* Procedure specific fields */;
    cv_int_t n // Procedure argument;
    cv_int_t *r // Procedure argument;
    cv_int_t *r1 // Declared at line 22, column 5 in file Fib.hs;
    cv_int_t *r2 // Declared at line 23, column 5 in file Fib.hs;
} rar_myfib_t;

rar_myfib_t *enter_myfib(rar_t *caller, uint32_t priority, uint8_t depth, int n, cv_int_t *r) {
    rar_myfib_t *rar = (rar_myfib_t *) enter(sizeof(rar_myfib_t), step_myfib, caller, priority, depth);
    initialise_int(&rar->n, n);
    rar->r = r;
    rar->r1 = (cv_int_t *) malloc(sizeof(cv_int_t));
    rar->r2 = (cv_int_t *) malloc(sizeof(cv_int_t));
};

void step_myfib(rar_t *gen_rar) {
    rar_myfib_t *rar = (rar_myfib_t *) gen_rar;
    switch(rar->pc) {
        case 0:
            initialise_int(rar->r1, 0);
            initialise_int(rar->r2, 0);
            if (!((rar->n.value) < (2))) goto L0;
            later_int(rar->r, now + 1, 1);
            goto L1;

        L0:
            uint8_t new_depth = rar->depth - 1;
            uint32_t pinc = 1 << new_depth;
            uint32_t new_priority = rar->priority;
            fork((rar_t *) enter_myfib( (rar_t *) rar
                                      , new_priority
                                      , new_depth
                                      , (rar->n.value) - (1)
                                      , rar->r1));
            new_priority += pinc;
            fork((rar_t *) enter_myfib( (rar_t *) rar
                                      , new_priority
                                      , new_depth
                                      , (rar->n.value) - (2)
                                      , rar->r2));
            new_priority += pinc;
            fork((rar_t *) enter_mysum( (rar_t *) rar
                                      , new_priority
                                      , new_depth
                                      , rar->r1
                                      , rar->r2
                                      , rar->r));
            rar->pc = 1;
            return;

        case 1:

        L1:

        free(rar->r1);
        free(rar->r2);
        leave((rar_t *) rar, sizeof(rar_myfib_t));
    }
}

