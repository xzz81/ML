SVM
1. 边界条件    margin
2. 超平面 hyperplane
3. 分类规则 classification rules
4. Estimating hyperplane(estimating $\omega,b $)
to minimize the margin
$$2\delta = \frac{1}{||\omega||}\omega ^T(x_1 -x_2)$$
 when 
$$\omega ^Tx +b= \pm1$$
we just consider
$$\underset{\omega , b}{\max } \frac{1}{||\omega||}$$
equals to
$$\underset{\omega ,b}{\min} ||\omega||$$
         $$Subject to g_i(\mathbf{w}^\top \mathbf{x} + b) \geq 1, \quad \forall i = 1, \ldots, n$$


Then we consider  convex optimization and Lagrange function
