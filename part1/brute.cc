#include <algorithm>
#include <array>
#include <iostream>
using namespace std;

enum nation_t { england, spain, ukraine, norway, japan };
enum color_t { red, green, ivory, yellow, blue };
enum pet_t { dog, snails, fox, horse, zebra };
enum drink_t { coffee, tea, milk, juice, water };
enum cigs_t {  old_gold, kool, chesterfield, lucky_strike, parliament };

template <typename T>
static bool next_to(array<T,5> attr, int i, T v)
{
  return (i <= 0 || attr[i-1] != v) && (i >= 4 || attr[i+1] != v);
}

template <typename A,typename B>
static bool both(int i, array<A,5>& ka, const A& va, array<B,5>& kb, const B& vb)
{
  return ka[i] == va && kb[i] != vb;
}

int main()
{
  array<nation_t, 5> nation;
  array<color_t,  5> color;
  array<pet_t,    5> pet;
  array<drink_t,  5> drink;
  array<cigs_t,   5> cigs;

  for (int i = 0; i < 5; i++)
    // This is horrible.  So I *LOVE* IT!!!
    // Try doing /that/ with std::iota!
    nation[i] = (nation_t) (
    color[i] = (color_t) (
    pet[i] = (pet_t) (
    drink[i] = (drink_t) (
    cigs[i] = (cigs_t) i))));

  for (;;) {
    int water_drinker, zebra_owner;

    // check the state against all constraints
    for (int i = 0; i < 5; i++) {
      if (both(i, nation, england, color, red))                 goto inconsistent;
      if (both(i, pet, dog, nation, spain))                     goto inconsistent;
      if (both(i, color, green, drink, coffee))                 goto inconsistent;
      if (both(i, nation, ukraine, drink, tea))                 goto inconsistent;
      if (color[i] == ivory && (i >= 4 || color[i+1] != green)) goto inconsistent;
      if (both(i, cigs, old_gold, pet, snails))                 goto inconsistent;
      if (both(i, cigs, kool, color, yellow))                   goto inconsistent;
      if (drink[i]  == milk && i != 2)                          goto inconsistent;
      if (i == 0 && nation[i] != norway)                        goto inconsistent;
      if (cigs[i] == chesterfield && next_to(pet,i,fox))        goto inconsistent;
      if (pet[i] == horse         && next_to(cigs,i,kool))      goto inconsistent;
      if (both(i, cigs, lucky_strike, drink, juice))            goto inconsistent;
      if (both(i, nation, japan, cigs, parliament))             goto inconsistent;
      if (nation[i] == norway     && next_to(color,i,blue))     goto inconsistent;

      if (drink[i] == water) water_drinker = nation[i];
      if (pet[i]   == zebra) zebra_owner   = nation[i];
    }
    cout << "Found solution: " << water_drinker << ", " << zebra_owner << endl;
    /* fall through */
  inconsistent:

    // permute to next state
    if (!next_permutation(nation.begin(), nation.end()) &&
        !next_permutation(color.begin(), color.end())   &&
        !next_permutation(pet.begin(), pet.end())       &&
        !next_permutation(drink.begin(), drink.end())   &&
        !next_permutation(cigs.begin(), cigs.end()))
      goto lose; // no next state :-(
  }
  return 0;

 lose:
  cout << "No other solution found.\n";
  return 1;
}
