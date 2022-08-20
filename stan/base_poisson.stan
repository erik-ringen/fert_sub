data{
    int N_obs;
    int N_id;
    int N_pop;
    array[N_id] int pop_id;
    array[N_obs] int pid;
    vector[N_obs] age;
    array[N_obs] int live_births;
    vector[N_id] birthyear_s;
}

parameters {
    // Global mean parameters //
    real a_alpha; // avg alpha
    real a_k; // avg k
    real a_b; // avg b
    
    real b_BY; // avg effect of birth year
    
    vector[N_pop] pop_BY_z;
    real<lower=0> sigma_pop_BY;
    
    // Random effects //
    matrix[N_id,2] pid_z; // individual-level random effects, unscaled and uncorrelated
    vector<lower=0>[2] sigma_pid; // scale par for pid

    matrix[N_pop,3] pop_z; // population-level random effects
    vector<lower=0>[3] sigma_pop; // 
}

transformed parameters{
  matrix[N_id,2] pid_v;
  matrix[N_pop,3] pop_v;
  vector[N_pop] pop_BY_v;
  
  vector[N_id] k;
  vector[N_id] b;
  vector[N_id] alpha;

  // Scale random effects //
  for (n in 1:N_id)
    for (j in 1:2 ) {
  pid_v[n,j] = pid_z[n,j] * sigma_pid[j];
    }
  
  for (j in 1:3) pop_v[,j] = pop_z[,j] * sigma_pop[j];

  pop_BY_v = pop_BY_z * sigma_pop_BY;

  // Compose parameters
  for (n in 1:N_id) {
    k[n] = exp(a_k + pop_v[pop_id[n],1] + pid_v[n,1]);
    b[n] = exp(a_b + pop_v[pop_id[n],2]);
    alpha[n] = exp(a_alpha + pop_v[pop_id[n],3] + pid_v[n,2] + (b_BY + pop_BY_v[pop_id[n]])*birthyear_s[n]);
  }
} // end transformed parameters block

model{
  //// Priors ////
  // Overall intercepts
  a_k ~ normal(0,2); 
  a_b ~ normal(0,2);
  a_alpha ~ normal(0,2);

  // Overall effect of birth year
  b_BY ~ std_normal(); // std_normal = Normal(0,1)

  // varying effects, unscaled
  pop_BY_z ~ std_normal();
  to_vector(pid_z) ~ std_normal();
  to_vector(pop_z) ~ std_normal();

  // variance components (half normal)
  sigma_pid ~ std_normal();
  sigma_pop ~ std_normal();
  sigma_pop_BY ~ std_normal();

  // likelihood
        for (i in 1:N_obs) { 
        real fert_cu;

        fert_cu = pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]]) * alpha[pid[i]];
        live_births[i] ~ poisson(fert_cu);
    }
}

generated quantities {
   vector[N_obs] live_births_hat;

   for (i in 1:N_obs) {
    real fert_cu;

    fert_cu = pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]]) * alpha[pid[i]];
    live_births_hat[i] = poisson_rng(fert_cu);
   }
}
