# LOD2021
Code for paper 'Using Statistical and Artificial Neural Networks Meta-learning Approaches for Uncertainty Isolation in Face Recognition by the Established Convolutional Models'

List of files:

 - Program files to re-train SOTA CNN models and train SNN:
Inception3Detail_bc2msr.m	
AlexNetDetail_bc2msr.m		
Vgg19Detail_bc2msr.m
GoogleNetDetail_bc2msr.m	
IResnet2Detail_bc2msr.m		
Resnet50Detail_bc2msr.m

To configure, find and modify the following fragment:
  %% CONFIGURATION PARAMETERS:
  % Download BookClub dataset from: https://data.mendeley.com/datasets/yfx9h649wz/3
  % and unarchive it into the dierctory below:
  %% Dataset root folder template and suffix
  dataFolderTmpl = '~/data/BC2_Sfx';
  dataFolderSfx = '1072x712';
  %Set number of models in the ensemble: 1, 2, 4, 8, 16
  nModels = 1;
  %Set directory and template for the retrained CNN models:
  save_net_fileT = '~/data/in_swarm';

 - Libraries:
   * Training and test sets building
createBCtestIDSvect6b1.m
createBCtestIDSvect6b.m
createBCbaselineIDS6b.m

   * Image size conversion:
readFunctionTrainGN_n.m
readFunctionTrainIN_n.m
readFunctionTrain_n.m

 - Summary per session results for single CNN models:
results_an_6bmsr1.txt
results_gn_6bmsr1.txt
results_in_6bmsr1.txt
results_ir_6bmsr1.txt
results_rn_6bmsr1.txt
results_vgg_6bmsr1.txt

 - Detailed per image results for Inception v.3 ensemble of 1, 2, 4, 8, 16 count:
predict_in_6bmsr1.txt
predict_in_6bmsr2.txt
predict_in_6bmsr4.txt
predict_in_6bmsr8.txt
predict_in_6bmsr16.txt

 - Detailed per image results for other CNN ensebles of count 4:
predict_an_6bmsr4.txt
predict_gn_6bmsr4.txt
predict_ir_6bmsr4.txt
predict_rn_6bmsr4.txt
predict_vgg_6bmsr4.txt

 - Accuracy metrics calculation script:
pred_dist2msr.R
pred_dist2lod.R
