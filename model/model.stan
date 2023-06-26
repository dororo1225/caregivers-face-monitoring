data {
  int<lower=0> N_obs;  // number of observation
  int<lower=0> N_pair; // number of infant-caregiver pair
  int<lower=0> Y[N_obs]; // number of images with detected face
  int<lower=0> N_img[N_obs]; // number of total images
  real X[N_obs]; // age in months
  int<lower=1, upper=N_pair> ID_pair[N_obs];
  int<lower=0> N_coding; // number of codings
  real q_detect_obs[N_coding]; // observed hit rate
  int<lower=0, upper=N_obs> ID_obs[N_coding]; 
  int N_predict;
  real X_predict[N_predict];
  int<lower=1, upper=N_pair> ID_pair_pred[N_predict];
  int N_predict_glob;
  real X_predict_glob[N_predict_glob];
}

parameters {
  real beta0;
  real beta1;
  vector<lower = 0>[2]  tau_u;  
  matrix[N_pair, 2] u;
  corr_matrix[2] rho_u;
  vector<lower=0, upper=1> [N_obs] q_detect;
  real<lower=0> sigma_q;
}

transformed parameters {
  real<lower=0, upper=1> q_face[N_obs];
  real<lower=0, upper=1> q[N_obs];
  for (i in 1:N_obs){
    q_face[i] = inv_logit(beta0 + (beta1 + u[ID_pair[i], 2]) * X[i] + u[ID_pair[i], 1]);
    q[i] = q_face[i] * q_detect[i];
  }
}

model {
  target += binomial_lpmf(Y | N_img, q);
  target += normal_lpdf(q_detect_obs | q_detect[ID_obs], sigma_q);
  target += student_t_lpdf(tau_u[1] | 3, 0, 2.5) - student_t_lccdf(0 | 3, 0, 2.5);
  target += student_t_lpdf(tau_u[2] | 3, 0, 2.5) - student_t_lccdf(0 | 3, 0, 2.5);
  target += lkj_corr_lpdf(rho_u | 1);
  target += student_t_lpdf(sigma_q | 3, 0, 2.5) - student_t_lccdf(0 | 3, 0, 2.5);
  for(i in 1:N_pair){
    target +=  multi_normal_lpdf(u[i,] | rep_row_vector(0, 2), quad_form_diag(rho_u, tau_u));
    }
}

generated quantities{
  real beta1_local[N_pair];
  real<lower=0, upper=1> q_face_pred[N_predict];
  real<lower=0, upper=1> q_face_pred_glob[N_predict_glob];

  for (j in 1:N_pair){
    beta1_local[j] = beta1 + u[j, 2];
  }

  for (k in 1:N_predict){
    q_face_pred[k] = inv_logit(beta0 + (beta1 + u[ID_pair_pred[k], 2]) * X_predict[k] + u[ID_pair_pred[k], 1]);
  }
  
  for (l in 1:N_predict_glob){
    q_face_pred_glob[l] = inv_logit(beta0 + beta1  * X_predict_glob[l]);
  }
}
