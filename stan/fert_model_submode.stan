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
    real a_p; // avg p
    
    vector[1] b_BY; // avg effect of birth year
    
    matrix[N_pop, 1] pop_BY_z;
    vector<lower=0>[1] sigma_pop_BY;
    
    // Random effects //
    matrix[N_id, 3] pid_z; // individual-level random effects, unscaled and uncorrelated
    vector[3] a_sigma_pid; // scale par for pid, avg
    
    matrix[N_pop, 3] pid_pop_z;
    vector<lower=0>[3] sigma_pop_pid; // variance in the variance...

    matrix[N_pop, 4] pop_z; // population-level random effects
    vector<lower=0>[4] sigma_pop; // 
    
    matrix[N_sub, 3] sub_z; // subtype random effects
    vector<lower=0>[3] sigma_sub;
}

transformed parameters{
  matrix[N_id, 3] pid_v;
  matrix[N_pop, 4] pop_v;
  matrix[N_pop, 1] pop_BY_v;
  matrix[N_sub, 3] sub_v;
  
  // Scale random effects //
  for (n in 1:N_id)
  for (j in 1:3) {
  pid_v[n, j] = pid_z[n, j] * exp(a_sigma_pid[j] + pid_pop_z[pop_id[n], j]*sigma_pop_pid[j]);
  }
  
  for (j in 1:4) pop_v[, j] = pop_z[, j] * sigma_pop[j];
  pop_BY_v[, 1] = pop_BY_z[, 1] * sigma_pop_BY[1];
  
  for (j in 1:3) sub_v[, j] = sub_z[, j] * sigma_sub[j];
  
} // end transformed parameters block

model{
    vector[N_id] k;
    vector[N_id] b;
    vector[N_id] alpha;
    vector[N_id] p;
    
    // Set priors //
    a_k ~ std_normal(); // std_normal = Normal(0,1)
    a_b ~ std_normal();
    a_p ~ std_normal();
    a_alpha ~ std_normal();
    
    b_BY ~ std_normal();
    to_vector(pop_BY_z) ~ std_normal();
    sigma_pop_BY ~ std_normal();

    to_vector(pid_z) ~ std_normal();
    to_vector(pid_pop_z) ~ std_normal();
    to_vector(pop_z) ~ std_normal();
    to_vector(sub_z) ~ std_normal();

    // variance components
    a_sigma_pid ~ std_normal();
    sigma_pop_pid ~ std_normal();
    sigma_pop ~ std_normal();
    sigma_sub ~ std_normal();

  // Compose parameters
  for (n in 1:N_id) {
    k[n] = exp(a_k + pop_v[pop_id[n], 1] + pid_v[n, 1] + sub_v[sub_id[n], 1]);
    
    b[n] = exp(a_b + pop_v[pop_id[n], 2] + pid_v[n, 2] + sub_v[sub_id[n], 2]);
    
    alpha[n] = exp(a_alpha + pop_v[pop_id[n], 3] + pid_v[n, 3] + sub_v[sub_id[n], 3] + (b_BY[1] + pop_BY_v[pop_id[n], 1])*birthyear_s[n]);
    
    p[n] = inv_logit(a_p + pop_v[pop_id[n], 4]);
  }
    
        for (i in 1:N_obs) { 
        real fert_cu;

        fert_cu = (pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]])) * alpha[pid[i]];

        if (live_births[i] == 0) {
            target += log_sum_exp(bernoulli_lpmf(1 | p[pid[i]]), bernoulli_lpmf(0 | p[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu));
        }

        else {
            target += bernoulli_lpmf(0 | p[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu);
        }
    }
}
