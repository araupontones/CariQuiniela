function f=laliga2(a,y1,y2,x11,x21,x12,x22,x13,x23,x14,x24)

ratio1=x12./x22;
ratio2=x22./x12;

lam1=exp(a(1) + a(2)*log(ratio1) + a(3)*x11 + a(4)*log((1-x13)./(1-x23)) + a(5)*log((x14+100)./(x24+100)));
lam2=exp(a(1) + a(2)*log(ratio2) + a(3)*x21 + a(4)*log((1-x23)./(1-x13)) + a(5)*log((x24+100)./(x14+100)));

hei=-log(gamma(y1+1)) - lam1 + y1.*log(lam1) - log(gamma(y2+1)) - lam2 + y2.*log(lam2);
f=sum(hei)*-1;
end