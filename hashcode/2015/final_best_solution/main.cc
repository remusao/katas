#include <algorithm>
#include <cstdio>
#include <vector>

using namespace std;

#define MAXR 77
#define MAXC 302
#define MAXL 2252
#define MAXA 11
#define MAXT 402
#define MAXB 54

int R, C, A, L, V, B, T, rs, cs;

struct Pt{
  int r, c;
  Pt(int r=0, int c=0) : r(r), c(c) {}
};

// global variables: auto-initalized, can be larger than what fits in stack
vector<int> cov[MAXR][MAXC]; // which targets are covered by a loon at (r, c)
Pt dest[MAXA][MAXR][MAXC]; // where a loon at (a, r, c) flies; (-1, -1) = out
Pt target[MAXL]; // position of target cells
bool covered[MAXT][MAXL]; // at time t, is target cell l covered yet?
// dynamic programming: best score at (t, a, r, c) and best direction
int best[MAXT][MAXA][MAXR][MAXC], dir[MAXT][MAXA][MAXR][MAXC];
int left[MAXT][MAXR][MAXC]; // how many new cells covered at (r, c) at time t
// solution: at time t, altitude adjustment for loon b 
int solution[MAXT][MAXB];

// given by problem statement
int columndist(int c1, int c2) { return min(abs(c1 - c2), C - abs(c1 - c2)); }
// does a loon at (r, c) cover target cell l?
int iscovered(int l, int r, int c) {
  int u = target[l].r, v = target[l].c;
  return ((r - u) * (r - u) + columndist(c, v) * columndist(c, v) <= V*V);
}

// return next move for loon b at time t when at position (a, r, c)...
// ... using the results of dynamic programming
int decide_dyn(int b, int t, int a, int r, int c) { return dir[t][a][r][c]; }
// ... using the computed solution
int decide_sol(int b, int t, int a, int r, int c) { return solution[t][b]; }

int simulate(int b, int (*decide)(int, int, int, int, int)) {
  // compute the run of loon b using decision function decide
  // return the score improvement
  int score = 0;
  int r = rs, c = cs, a = 0;
  for (int t = 0; t < T; t++) {
    int bestda = (*decide)(b, t, a, r, c);
    // store the move
    solution[t][b] = bestda;
    // apply it
    a += bestda;
    Pt next = dest[a][r][c];
    r = next.r;
    c = next.c;
    if (r < 0)
      break; // loon exited
    if (a > 0) {
      // update covered target cells
      for (unsigned int i = 0; i < cov[r][c].size(); i++) {
        int l = cov[r][c][i];
        // score improved if target cell not already covered
        score += covered[t+1][l] ? 0 : 1;
        covered[t+1][l] = true;
      }
    }
  }
  return score;
}

void route(int b) {
  // route loon b given the already covered targets (in covered)

  // save a factor A: pre-count the uncovered targets for each cell
  for (int t = 0; t <= T; t++)
    for (int r = 0; r < R; r++)
      for (int c = 0; c < C; c++) {
        int score = 0;
        for (unsigned int i = 0; i < cov[r][c].size(); i++) {
          int l = cov[r][c][i];
          score += covered[t][l] ? 0 : 1;
        }
        left[t][r][c] = score;
      }

  // dynamic programming, t == T is a sentinel value
  for (int t = T-1; t >= 0; t--)
    for (int a = 0; a <= A; a++)
      for (int r = 0; r < R; r++)
        for (int c = 0; c < C; c++) {
          int bestda = 0, bestv = 0; // best altitude adjustment, best value
          for (int da = -1; da <= 1; da++) {
            if (a <= 1 && da == -1)
              continue; // can't go down when on ground, or back on ground
            if (a + da > A)
              continue; // can't go above max altitude
            // pretend we moved b by da at time t
            Pt next = dest[a + da][r][c];
            if (next.r < 0)
              break; // loon goes out, so score remains 0
            // score if going at these new coordinates, computed dynamically
            int rscore = best[t+1][a+da][next.r][next.c];
            // if above ground, score increased by number of covered targets
            if ((a + da) > 0)
              rscore += left[t+1][next.r][next.c];
            // break ties at random
            if (rscore > bestv || (rscore == bestv && (rand() % 2))) {
              // da is the best altitude adjustment
              bestv = rscore;
              bestda = da;
            }
          }
          // store best altitude adjustment, best value
          best[t][a][r][c] = bestv;
          dir[t][a][r][c] = bestda;
        }
}

void print_sol() {
  FILE *f = fopen("output", "w");
  for (int t = 0; t < T; t++) {
    for (int b = 0; b < B; b++)
      fprintf(f, "%d ", solution[t][b]);
    fprintf(f, "\n");
  }
  fclose(f);
}

int main(int argc, char **argv) {
  srand(42); // make it deterministic
  scanf("%d%d%d", &R, &C, &A);
  scanf("%d%d%d%d", &L, &V, &B, &T);
  scanf("%d%d", &rs, &cs);
  for (int l = 0; l < L; l++) {
    int r, c;
    scanf("%d%d", &r, &c);
    target[l] = Pt(r, c);
  }
  for (int r = 0; r < R; r++)
    for (int c = 0; c < C; c++) {
    }
  for (int a = 0; a <= A; a++)
    for (int r = 0; r < R; r++)
      for (int c = 0; c < C; c++)
        if (!a) {
          // wind at altitude 0 does not move loons
          dest[a][r][c] = Pt(r, c);
        } else {
          int dr, dc;
          scanf("%d%d", &dr, &dc);
          // populate dest with the coordinates where the wind takes the loon
          int destr, destc;
          destr = r + dr;
          destc = (c + dc + C) % C;
          if (destr < 0 || destr >= R)
            destr = destc = -1; // loon goes out
          dest[a][r][c] = Pt(destr, destc);
        }

  // compute for each cell the list of targets covered by a loon at this cell
  for (int r = 0; r < R; r++)
    for (int c = 0; c < C; c++)
      for (int l = 0; l < L; l++)
        if (iscovered(l, r, c))
          cov[r][c].push_back(l);

  int orig_score = 0;
  // iteratively compute a route for each loon given other routes
  while (true) {
    for (int br = 0; br < B; br++) {
      // route br given the routes of the other loons
      // first, forget about which cells are covered
      for (int t = 0; t <= T; t++)
        for (int l = 0; l < L; l++)
          covered[t][l] = false;
      // then, simulate the other loons (default routes are 0 ... 0)
      int score = 0;
      for (int b = 0; b < B; b++)
        if (b != br)
          score += simulate(b, &decide_sol);
      // now, compute the route of br with dynamic programming
      route(br);
      score += simulate(br, &decide_dyn);
      fprintf(stderr, "route %d\n", br);
      if (score > orig_score) {
        fprintf(stderr, "score was %d is %d\n", orig_score, score);
        orig_score = score;
        print_sol();
      }
    }
  }

  return 0;
}

