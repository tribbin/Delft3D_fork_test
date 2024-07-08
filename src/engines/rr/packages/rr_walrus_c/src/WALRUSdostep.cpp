#include "WALRUS.hh"

/*! @file
 * @brief implements the actual step (with substeps)
 */

void WALRUS::dostep(double deltime) {
        // initialize
        double timeend = _time + deltime;
        unsigned int stepcounter = 0;
        cur_fXG_c = 0.0;
        cur_fXS_c = 0.0;
        cur_PQ_c = 0.0;
        cur_PV_c = 0.0;
        cur_PS_c = 0.0;
        cur_ETV_c = 0.0;
        cur_ETS_c = 0.0;
        cur_ETact_c = 0.0;
        cur_fXSact_c = 0.0;
        cur_fQS_c = 0.0;
        cur_fGS_c = 0.0;
        cur_Q_c = 0.0;
        cur_P_c = 0.0;
        cur_ETpot_c = 0.0;
        calctryvalues(timeend);
        double tc = pred_time_tries_OK(timeend);
        while ((tc + min_deltime < timeend) & (stepcounter < max_substeps)) {
                stepcounter++;
                double t1 = tc;
                calctryvalues(t1);
                tc = pred_time_tries_OK(t1);
                while ((tc + min_deltime < t1) & (stepcounter < max_substeps)) {
                        t1 = tc;
                        stepcounter++;
                        calctryvalues(t1);
                        tc = pred_time_tries_OK(t1);
                }
                // accept try values until t1
                _time = t1;
                _dV = dV_try;
                _hQ = hQ_try;
                _hS = hS_try;
                _dG = dG_try;
                cur_fXG_c += fXG_try;
                cur_fXS_c += fXS_try;
                cur_PQ_c += PQ_try;
                cur_PV_c += PV_try;
                cur_ETV_c += ETV_try;
                cur_ETS_c += ETS_try;
                cur_ETact_c += ETact_try;
                cur_fXSact_c += fXSact_try;
                cur_fQS_c += fQS_try;
                cur_fGS_c += fGS_try;
                cur_Q_c += Q_try;
                // added GP
                cur_P_c += P_try;
                cur_ETpot_c += ETpot_try;
                // setup next part of the step
                calctryvalues(timeend);
                tc = pred_time_tries_OK(timeend);
        }
        // finalize
        _time = timeend;
        _dV = dV_try;
        _hQ = hQ_try;
        _hS = hS_try;
        _dG = dG_try;
        cur_fXG_c += fXG_try;
        cur_fXS_c += fXS_try;
        cur_PQ_c += PQ_try;
        cur_PV_c += PV_try;
        cur_ETV_c += ETV_try;
        cur_ETS_c += ETS_try;
        cur_ETact_c += ETact_try;
        cur_fXSact_c += fXSact_try;
        cur_fQS_c += fQS_try;
        cur_fGS_c += fGS_try;
        cur_Q_c += Q_try;
        // added GP
        cur_P_c += P_try;
        cur_ETpot_c += ETpot_try;
        _lastdeltime = deltime;
}

void WALRUS::dosteps(double endtime, double deltime,
                      const char* csvfilename,
                      bool store_states = true,
                      bool store_deps = true,
                      bool store_usedfluxes = true)
{
        ofstream resultfile;
        resultfile.open(csvfilename);
        resultfile.precision(9);
        resultfile<<"time";
        if (store_states) {
                resultfile<<", dV";
                resultfile<<", dG";
                resultfile<<", hQ";
                resultfile<<", hS";
        }
        if (store_deps) {
                resultfile<<", W";
                resultfile<<", beta";
                resultfile<<", dVeq";
        }
        if (store_usedfluxes) {
                resultfile<<", fXG";
                resultfile<<", fXS";
                resultfile<<", PQ";
                resultfile<<", PV";
                resultfile<<", PS";
                resultfile<<", ETV";
                resultfile<<", ETS";
                resultfile<<", ETact";
                resultfile<<", fXSact";
                resultfile<<", fQS";
                resultfile<<", fGS";
                resultfile<<", Q";
                resultfile<<", Qdischarge";
        }
        resultfile<<endl;

        while(_time < endtime) {
                dostep(deltime);
                resultfile<< _time;
                if (store_states) {
                        resultfile<<", "<< _dV;
                        resultfile<<", "<< _dG;;
                        resultfile<<", "<< _hQ;
                        resultfile<<", "<< _hS;
                }
                if (store_deps) {
                        resultfile<<", "<< get(cur_W);
                        resultfile<<", "<< get(cur_beta);
                        resultfile<<", "<< get(cur_dVeq);
                }
                if (store_usedfluxes) {
                        resultfile<<", "<< cur_fXG_c;
                        resultfile<<", "<< cur_fXS_c;
                        resultfile<<", "<< cur_PQ_c;
                        resultfile<<", "<< cur_PV_c;
                        resultfile<<", "<< cur_PS_c;
                        resultfile<<", "<< cur_ETV_c;
                        resultfile<<", "<< cur_ETS_c;
                        resultfile<<", "<< cur_ETact_c;
                        resultfile<<", "<< cur_fXSact_c;
                        resultfile<<", "<< cur_fQS_c;
                        resultfile<<", "<< cur_fGS_c;
                        resultfile<<", "<< cur_Q_c;
                        resultfile<<", "<< get(last_Qdischarge);
                }
                resultfile<<endl;
        }
        resultfile.close();
}
