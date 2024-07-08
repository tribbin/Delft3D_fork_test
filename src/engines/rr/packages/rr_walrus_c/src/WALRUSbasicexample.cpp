#include <fstream>
#include <iostream>

#include "WALRUS.hh"

using namespace std;

int main(void) {

  WALRUS Wvalid;

  // read forcing input
  ifstream inFile("validinput.dat", std::ifstream::in);
  vector<double> forcing_time;
  vector<double> forcing_P;
  vector<double> forcing_ETpot;
  double last_time, last_P, last_ETpot, dummy;
  bool donereading = false;
  while (!donereading) {
    inFile >> last_time >> last_P >> last_ETpot >> dummy;
    donereading = inFile.eof();
    if (!donereading) {
      forcing_time.push_back(last_time);
      forcing_P.push_back(last_P);
      forcing_ETpot.push_back(last_ETpot);
    }
  }
  double t_begin = forcing_time[0];
  double t_end = forcing_time[forcing_time.size() - 1];
  // set forcing
  Wvalid.set_seq_C(fc_P, forcing_time, forcing_P);
  Wvalid.set_seq_C(fc_ETpot, forcing_time, forcing_ETpot);
  // set parameters
  Wvalid.set(par_cW, 200);
  Wvalid.set(par_cV, 4);
  Wvalid.set(par_cG, 5e6);
  Wvalid.set(par_cQ, 10);
  Wvalid.set(par_cS, 4);
  Wvalid.set(par_cD, 1500);
  Wvalid.set(par_aS, 0.01);
  Wvalid.set_st(loamy_sand);

  // iniitialize model
  Wvalid.init(0.0044, NAN, 1250, NAN, NAN);
  Wvalid.set(cur_time, t_begin);
  // calculate and save data
  Wvalid.dosteps(t_end, 3600, "WALRUSresult.csv", true, false, false);

  return 0;
}
