
#include "stdio.h"


// Simplify
int run2() {
    int b = 0,
        c = 0,
        d = 0,
        e = 0,
        f = 0,
        g = 0,
        h = 0;

    b = 109300;
    c = 126300;

    while (1) {
        f = 1; d = 2;

        do {
            e = 2;

            while (e < b) {
                if (d * e == b) {
                    f = 0;
                    goto inc;
                }
                ++e;
            }

            g = ++d - b;

            // printf("b = %d; c = %d; d = %d; e = %d, f = %d; g = %d;\n", b, c, d, e, f, g);
        } while (g != 0);
        // g = 0

        // printf("f = %d\n", f);
        if (f != 0) goto jump26; // 24: fjnz f 2
        inc: ++h;
        printf("h = %d\n", h);

        jump26: g = b - c;
        // printf("g = %d\n", g);

        if (g != 0) goto jump30; // 28: fjnz g 2
        goto jump32; // 29: fjnz 1 3
        jump30: b -= -17;
    }

jump32: return h;
}

int main() {
    printf("Result: %d", run2());
    return 0;
}
