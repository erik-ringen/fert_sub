functions{
  /* compute monotonic effects, function taken from brms package
  * Args:
    *   scale: a simplex parameter
  *   i: index to sum over the simplex
  * Returns:
    *   a scalar between 0 and 1
  */
    real mo(vector scale, int i) {
      if (i == 1) {
        return 0;
      } 
      else {
        return sum(scale[1:(i-1)]);
      }
    }
}

data{
    int N_obs;
    int N_id;
    int N_pop;
    array[N_id] int pop_id;
    array[N_obs] int pid;
    vector[N_obs] age;
    array[N_obs] int live_births;
    vector[N_id] birthyear_s;
    array[N_id] int ord_pred;
    int K;
}

parameters {
    // Global mean parameters //
    real a_alpha; // avg alpha
    real a_k; // avg k
    real a_b; // avg b
    real a_p; // avg p
    
    vector[1] b_BY; // avg effect of birth year
    
    matrix[N_pop,1] pop_BY_z;
    vector<lower=0>[1] sigma_pop_BY;
    
    // Monotonic effects //
    vector[3] b_ord; // avg effect
    matrix[K-1,3] a_scale; // global intercepts for monotonic scale
    
    // Random effects //
    matrix[N_id,3] pid_z; // individual-level random effects, unscaled and uncorrelated
    vector[3] a_sigma_pid; // scale par for pid, avg
    
    matrix[N_pop,3] pid_pop_z;
    vector<lower=0>[3] sigma_pop_pid; // variance in the variance...

    matrix[N_pop,(7 + 3*(K-1))] pop_z; // population-level random effects
    vector<lower=0>[(7 + 3*(K-1))] sigma_pop; // 
}

transformed parameters{
  matrix[N_id,3] pid_v;
  matrix[N_pop,(7 + 3*(K-1))] pop_v;
  matrix[N_pop,1] pop_BY_v;
  array[N_pop] matrix[K,3] pop_scale;
  
  // Scale random effects //
  for (n in 1:N_id)
  for (j in 1:3 ) {
  pid_v[n,j] = pid_z[n,j] * exp( a_sigma_pid[j] + pid_pop_z[pop_id[n],j]*sigma_pop_pid[j] );
  }
  
  for ( j in 1:(7 + 3*(K-1)) ) pop_v[,j] = pop_z[,j] * sigma_pop[j];
   pop_BY_v[,1] = pop_BY_z[,1] * sigma_pop_BY[1];
  
  // Monotonic effect scales, pop-specific //
  for (n in 1:N_pop) 
    for (j in 1:3) {
    vector[K] c;
    
    for (k in 1:(K-1)) c[k] = a_scale[k,j] + pop_v[n,(7 + (j-1)*(K-1) + k)]; 
    c[K] = 0; // ref category
    
    pop_scale[n,,j] = c;
  }
  
  
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
    
    b_ord ~ std_normal();
    to_vector(a_scale) ~ std_normal();

    to_vector(pid_z) ~ std_normal();
    to_vector(pid_pop_z) ~ std_normal();
    to_vector(pop_z) ~ std_normal();

    // variance components
    a_sigma_pid ~ std_normal();
    sigma_pop_pid ~ std_normal();
    sigma_pop ~ std_normal();

  // Compose parameters
  for (n in 1:N_id) {
    
    k[n] = exp( a_k + pop_v[pop_id[n],1] + pid_v[n,1] + (b_ord[1] + pop_v[pop_id[n],5])*mo(softmax(pop_scale[pop_id[n],,1]), ord_pred[n]) );
    
    b[n] = exp( a_b + pop_v[pop_id[n],2] + pid_v[n,2] + (b_ord[2] + pop_v[pop_id[n],6])*mo(softmax(pop_scale[pop_id[n],,2]), ord_pred[n]) );
    
    alpha[n] = exp( a_alpha + pop_v[pop_id[n],3] + pid_v[n,3] + (b_BY[1] + pop_BY_v[pop_id[n],1])*birthyear_s[n] + (b_ord[3] + pop_v[pop_id[n],7])*mo(softmax(pop_scale[pop_id[n],,3]), ord_pred[n])  );
    
    p[n] = inv_logit( a_p + pop_v[pop_id[n],4]  );
  }
    
        for (i in 1:N_obs) { 
          
        // If predictor not missing
        if (ord_pred[pid[i]] != -99) {
        real fert_cu;

        fert_cu = (pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]])) * alpha[pid[i]];

        if (live_births[i] == 0) {
            target += log_sum_exp(bernoulli_lpmf(1 | p[pid[i]]), bernoulli_lpmf(0 | p[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu));
        }

        else {
            target += bernoulli_lpmf(0 | p[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu);
        }
        
        }
  } // end loop over obs
  
} // end model block
