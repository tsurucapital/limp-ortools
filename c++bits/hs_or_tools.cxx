#define USE_GLOP

#include <google/protobuf/stubs/common.h>

#if GOOGLE_PROTOBUF_VERSION < 2006001
/* Hack: define GOOGLE_PROTOBUF_VERSION to avoid version mismatch error.
 * This is safe because we don't use protobuf.
 */
#undef GOOGLE_PROTOBUF_VERSION
#define GOOGLE_PROTOBUF_VERSION 2005001
#endif

#include "HsFFI.h"
#include "linear_solver/linear_solver.h"
#include "glop/lp_solver.h"
#include <string>
#include <iostream>

namespace {

/* for debugging */
void dump_model(operations_research::MPSolver &solver)
{
  std::string model;

  solver.ExportModelAsLpFormat(false, &model);
  std::cout << model << "\n";
}

}

extern "C" HsInt hs_ortools_solve_mip(
  HsInt nints, HsInt nreals, HsInt nctrs,
  HsInt n_obj_terms, HsInt n_ctr_terms,
  const HsInt *obj_vars, const HsDouble *obj_coeffs,
  const HsInt *ctr_ids, const HsInt *ctr_vars, const HsDouble *ctr_coeffs,
  const HsDouble *ctr_lower, const HsDouble *ctr_upper,
  const HsDouble *var_lower, const HsDouble *var_upper,
  HsInt *sol_ints, HsDouble *sol_reals)
{
  using namespace operations_research;

  const int nvars = nints + nreals;
  const std::string empty;

  MPSolver solver("hs_ortools_solve_mip", MPSolver::GLOP_LINEAR_PROGRAMMING);

  /* Set up variables */
  std::vector<MPVariable *> vars;

  for(int i = 0; i < nints; i++)
    vars.push_back(solver.MakeIntVar(var_lower[i], var_upper[i], empty));
  for(int i = nints; i < nvars; i++)
    vars.push_back(solver.MakeNumVar(var_lower[i], var_upper[i], empty));

  /* Set up objective */
  MPObjective *const objective = solver.MutableObjective();
  for(int i = 0; i < n_obj_terms; i++)
    objective->SetCoefficient(vars[obj_vars[i]], obj_coeffs[i]);
  objective->SetMinimization();

  /* Create constraints */
  std::vector<MPConstraint *> ctrs;

  for(int i = 0; i < nctrs; i++)
    ctrs.push_back(solver.MakeRowConstraint(ctr_lower[i], ctr_upper[i]));

  /* Set up constraint coefficients */
  for(int i = 0; i < n_ctr_terms; i++)
    ctrs[ctr_ids[i]]->SetCoefficient(vars[ctr_vars[i]], ctr_coeffs[i]);

  // dump_model(solver);

  /* Solve it! */
  const MPSolver::ResultStatus result_status = solver.Solve();

  for(int i = 0; i < nints; i++)
    sol_ints[i] = vars[i]->solution_value();
  for(int i = 0; i < nreals; i++)
    sol_reals[i] = vars[i + nints]->solution_value();

  switch(result_status)
  {
  case MPSolver::OPTIMAL: return 0;
  case MPSolver::INFEASIBLE: return 1;
  case MPSolver::UNBOUNDED: return 2;
  case MPSolver::ABNORMAL: return 3;
  default: return -1;
  }
}

extern "C" HsInt hs_glop_solve_mip(
  HsInt nints, HsInt nreals, HsInt nctrs,
  HsInt n_obj_terms, HsInt n_ctr_terms,
  const HsInt *obj_vars, const HsDouble *obj_coeffs,
  const HsInt *ctr_ids, const HsInt *ctr_vars, const HsDouble *ctr_coeffs,
  const HsDouble *ctr_lower, const HsDouble *ctr_upper,
  const HsDouble *var_lower, const HsDouble *var_upper,
  HsInt *sol_ints, HsDouble *sol_reals)
{
  using namespace operations_research;
  using namespace glop;

  const int nvars = nints + nreals;
  const std::string empty;

  LinearProgram prog;

  prog.SetName("hs_glop_solve_mip");

  /* Set up variables */
  std::vector<ColIndex> vars;

  for(int i = 0; i < nvars; i++)
  {
    ColIndex var = prog.CreateNewVariable();
    prog.SetVariableBounds(var, var_lower[i], var_upper[i]);
    prog.SetVariableIntegrality(var, i < nints);
    vars.push_back(var);
  }

  /* Set up objective */
  for(int i = 0; i < n_obj_terms; i++)
    prog.SetObjectiveCoefficient(vars[obj_vars[i]], obj_coeffs[i]);

  /* Create constraints */
  std::vector<RowIndex> ctrs;

  for(int i = 0; i < nctrs; i++)
  {
    RowIndex ctr = prog.CreateNewConstraint();
    prog.SetConstraintBounds(ctr, ctr_lower[i], ctr_upper[i]);
    ctrs.push_back(ctr);
  }

  /* Set up constraint coefficients */
  for(int i = 0; i < n_ctr_terms; i++)
    prog.SetCoefficient(ctrs[ctr_ids[i]], vars[ctr_vars[i]], ctr_coeffs[i]);

  // dump_model(solver);

  /* Solve it! */
  prog.CleanUp();
  LPSolver solver;
  const ProblemStatus result_status = solver.Solve(prog);

  for(int i = 0; i < nints; i++)
    sol_ints[i] = solver.variable_values()[vars[i]];
  for(int i = 0; i < nreals; i++)
    sol_reals[i] = solver.variable_values()[vars[i + nints]];

  switch(result_status)
  {
  case ProblemStatus::OPTIMAL:
    return 0;

  case ProblemStatus::PRIMAL_INFEASIBLE:
  case ProblemStatus::DUAL_UNBOUNDED:
    return 1;

  case ProblemStatus::PRIMAL_UNBOUNDED:
    return 2;

  case ProblemStatus::DUAL_INFEASIBLE:
  case ProblemStatus::INFEASIBLE_OR_UNBOUNDED:
  case ProblemStatus::ABNORMAL:
    return 3;

  case ProblemStatus::IMPRECISE:
    return 4;

  case ProblemStatus::INVALID_PROBLEM:
    return 5;

  default: return -(int)result_status;
  }
}
