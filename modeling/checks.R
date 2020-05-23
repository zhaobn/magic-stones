
# Note: reuse functions from simulations.R

# Task configs
features<-list()
features[['color']]<-c('b', 'r', 'y') # blue, red, yellow
features[['shape']]<-c('c', 'd', 's') # circle, diamond, square

obj_sep=''
all_objs<-get_all_objs(features)

learn01<-list()
learn01[['agent']]<-'rs'
learn01[['recipient']]<-'yc'
learn01[['result']]<-'ys'

# Override custom functions
read_f<-function(feature, obj) {
  f_idx<-if (feature=='color') 1 else 2
  return(substr(obj, f_idx, f_idx))
}

# Normative predictions
plot_pred_hm(get_cat_preds(learn01, 1, T))
plot_pred_hm(get_causal_pred(learn01, 1, T))












