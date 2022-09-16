data{
    int N_obs;
    int N_id;
    int N_pop;
    int N_sub;
    array[N_id] int pop_id;
    array[N_obs] int pid;
    vector[N_obs] age;
    array[N_obs] int live_births;
    vector[N_id] birthyear_s;
    array[N_id] int sub_id;
}

parameters {
    // Global mean parameters //
    real a_alpha; // avg alpha
    real a_k; // avg k
    real a_b; // avg b
    real a_oi;
    
    real b_BY; // avg effect of birth year
    
    matrix[N_sub, 2] sub_z; // subtype random effects
    vector[2] log_sigma_sub;
    
    vector[N_pop] pop_BY_z;
    real log_sigma_pop_BY;
    
    // Random effects //
    matrix[N_id,2] pid_z; // individual-level random effects, unscaled and uncorrelated
    vector[2] log_sigma_pid; // scale par for pid, log-scale for better sampling

    matrix[N_pop,4] pop_z; // population-level random effects
    vector[4] log_sigma_pop; // 
}

transformed parameters{
  matrix[N_id,2] pid_v;
  matrix[N_pop,4] pop_v;
  vector[N_pop] pop_BY_v;
  matrix[N_sub, 2] sub_v;
  
  vector[N_id] k;
  vector[N_id] b;
  vector[N_id] alpha;
  vector[N_id] oi;

  // Scale random effects //
  for (n in 1:N_id)
    for (j in 1:2 ) {
  pid_v[n,j] = pid_z[n,j] * exp(log_sigma_pid[j]);
    }
  
  for (j in 1:4) pop_v[,j] = pop_z[,j] * exp(log_sigma_pop[j]);

  pop_BY_v = pop_BY_z * exp(log_sigma_pop_BY);
  
  for (j in 1:2) sub_v[, j] = sub_z[, j] * exp(log_sigma_sub[j]);

  // Compose parameters
  for (n in 1:N_id) {
    k[n] = exp(a_k + pop_v[pop_id[n],1] + pid_v[n,1] + sub_v[sub_id[n], 1]);
    b[n] = exp(a_b + pop_v[pop_id[n],2]);
    alpha[n] = exp(a_alpha + pop_v[pop_id[n],3] + pid_v[n,2] + (b_BY + pop_BY_v[pop_id[n]])*birthyear_s[n] + sub_v[sub_id[n], 2]);
    oi[n] = inv_logit(a_oi + pop_v[pop_id[n],4]);
  }
} // end transformed parameters block

model{
  //// Priors ////
  // Overall intercepts
  a_k ~ normal(0,2); 
  a_b ~ normal(0,2);
  a_alpha ~ normal(0,2);
  a_oi ~ normal(0,2);

  // Overall effect of birth year
  b_BY ~ std_normal(); // std_normal = Normal(0,1)

  // varying effects, unscaled
  pop_BY_z ~ std_normal();
  to_vector(pid_z) ~ std_normal();
  to_vector(pop_z) ~ std_normal();
  to_vector(sub_z) ~ std_normal();

  // variance components (log scale)
  log_sigma_pid ~ normal(-1,1);
  log_sigma_pop ~ normal(-1,1);
  log_sigma_pop_BY ~ normal(-1,1);
  log_sigma_sub ~ normal(-1,1);

  // likelihood
        for (i in 1:N_obs) { 
        real fert_cu;

        fert_cu = pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]]) * alpha[pid[i]];
        
        if (live_births[i] == 1) {
            target += log_sum_exp(
            log(oi[pid[i]]),
            log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu)
          );
        }
        
        else {
          target += log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu);
        }
    }
}