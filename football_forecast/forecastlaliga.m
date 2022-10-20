% forecastlaliga is a model based on two independent poisson dristrubitions
% where the rate is model by the log of a linear function of regressors.
%[WL DRN LL MP bb bbint]=forecastlaliga(IndxL,IndxV,GPML,GPMV,FTSL,FTSV)
%
%   The explanatory variables are as follows:
%
%    IndxL = Cumulative difference, i.e. goals scored - goals recieved, for
%    the last two seassons for the local team.
%
%    IndxV = Cumulative difference, i.e. goals scored - goals recieved, for
%    the last two seassons for the visitor team.
%
%    GPML = Goals per match for the local team.
%    GPMV = Goals per match for the visitor team.
%
%    FTSL = Fail to score for the local team, i.e. in how many games the local
%    team did not scored a goal.
%
%    FTSV = Fail to score for the visitors team, i.e. in how many games the 
%    visitor team did not scored a goal.
%
%Parameters in the model are estimated by MLE. 
%forecastlaliga calls the algorithm @laliga.

function [WL DRN LL MPL MPV a bbint]=forecastlaliga(GPML,GPMV,FTSL,FTSV,TOTL,TOTV)

A=xlsread('football.xlsx','Sheet2','D2:M903');
y1=A(:,1);
y2=A(:,2);
x11=A(:,3);
x21=A(:,4);
x12=A(:,5);
x22=A(:,6);
x13=A(:,7);
x23=A(:,8);
x14=A(:,9);
x24=A(:,10);

w0=[-.01 .01 -.01 .01];
options = optimset('LargeScale','off');
[a,lmax,f,g,l,H0]=fminunc(@laliga,w0,options,y1,y2,x11,x21,x13,x23,x14,x24);
bbint=sqrt(diag(inv(H0)));

lam1=exp( a(1) + a(2)*GPML + a(3)*log((1-FTSL)./(1-FTSV)) + a(4)*log((TOTL+100)./(TOTV+100)) );
lam2=exp( a(1) + a(2)*GPMV + a(3)*log((1-FTSV)./(1-FTSL)) + a(4)*log((TOTV+100)./(TOTL+100)) );

prob1=poisspdf(0:50,lam1);
prob2=poisspdf(0:50,lam2);

DRN=sum(prob1.*prob2);

A1=repmat(prob1(2:end),50,1);
b2=prob2(1:50);
winL= bsxfun(@times,triu(A1),b2');

A2=repmat(prob2(2:end),50,1);
b1=prob1(1:50);
looseL= bsxfun(@times,triu(A2),b1');

WL=sum(winL(:)); 
LL=sum(looseL(:));
MPL=[winL(1:5,1:5);prob1(1:5).*prob2(1:5)];
MPV=[looseL(1:5,1:5);prob2(1:5).*prob1(1:5)];

end

