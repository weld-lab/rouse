#import "@preview/physica:0.9.5":*


// PARAMETERS
#set page(margin: (x:25mm, y:auto), numbering : "1")
#set text(font:"Times New Roman", size:12pt)
#set heading(numbering : "1.")
#set par(justify: true)
#set math.mat(delim: "[")
#set figure.caption(separator: [ --- ])

#set math.equation(numbering: "(1)")
#show math.equation: it => {
  if it.block and not it.has("label") [
    #counter(math.equation).update(v => v - 1)
    #math.equation(it.body, block: true, numbering: none)#label("")
  ] else {
    it
  }  
}


#show link: set text(fill: blue)

#show figure.caption: c => context [
  #text(weight: "bold")[#c.supplement #c.counter.display(c.numbering) #c.separator]	
  #c.body
]

// FILE DESC
#set document(
     title:"TO FILL",
     author:"Erwan Le Doeuff (weld)",
     date: datetime(year:2025, month:10, day:24)
)


#let appendix(body) = {
  set heading(numbering: "A.1.", supplement: [Appendix])
  counter(heading).update(0)
  body
}


#show raw.where(block: false): box.with(
  fill: rgb(0%, 30%, 100%, 10%),
  inset: (x: 3pt, y: 0pt),
  outset: (y: 4pt),
  radius: 3pt,
)


#show raw.where(block: true): box.with(
  width: 100%,
  fill: rgb(0%, 30%, 100%, 20%),
  inset: (x: 3pt, y: 0pt),
  outset: (y: 6pt),
  radius: 3pt,
)

//#text(17pt)[*Abstract*]
//To write.

#text(17pt)[*Plan*]

- Appendices
    - Installer common lisp (portacle) et le projet
    - Faire une video demonstrative (supplementaire)
    - Commentaire du code, github


#pagebreak()
#set page(columns:1, header:[`insert here`])

= Introduction

The mesoscale is, perhaps, one of the most difficult frontiers in physics. At least, conceptually it is for me. It lies in that ambiguous territory where neither the mathematical clarity of microscopic laws nor the comforting simplicity of macroscopic approximations can be fully trusted. It is a domain of negotiation, where intuition must constantly be rebuilt, and where the language of isolated equations gives way to that of collective behaviour.

On the microscopic side, mathematics often retains its authority. The systems are small enough, the variables are explicit and --- at least in principle --- the governing equations can be written down exactly. Quantum mechanics stands as the clearest example of this triumph: an austere yet complete framework capable of capturing atoms, molecules and the very first signs of disorder, provided one accepts the mental gymnastics it demands.

At the opposite extreme, the macroscopic world feels reassuringly coarse. Details fade into averages and fluctuations are smoothed out by numbers. We no longer track the motion of each component, but rather treat the entire ensemble as if it were a few single beings. We speak of global laws --- thermodynamics, hydrodynamics or even social dynamics. It is the scale of our daily experience, the one our senses and instincts have evolved to understand. We are creatures of the macroscopic world.

I know this is a personal take, but the mesoscopic scale seems to be where our confidence as physicists firstly fades. Our intuition, shaped by the macroscopic world we inhabit, keeps trying to tell us how things should behave. But at this scale, it is often wrong. The individuals --- the particles, the degrees of freedom --- still exist, still move, still collide. They have not yet dissolved into the smooth averages, and their presence makes every description painfully complex. Who would dare to write $10^24$ coupled equations to describe a gas? And yet, that is what the mesoscopic world silently demands of us: to think of the collective without ever forgetting the individual.


And yet, our predecessors have not faced this complexity empty-handed. Over time, we have learned to think differently --- to accept that not every variable deserves to be followed, that meaning can emerge from noise. The mesoscopic world has taught us to reason from the bottom up, to let the collective arise from the individual, and to translate collisions into probabilities. From these attempts were born the stochastic formalisms, the language of open systems, and the statistical methods that still guide us today --- as fragile, as approximated, it can be but still remarkably alive.

The work presented here follows the same lineage. We will begin with a chain in contact with a solvent --- or, to borrow the physicist's jargon, with a reservoir. The question is how this environment influences the chain, how it stirs, folds, and makes it vibrate. It is, in essence, a continuation of the ideas first explored by Langevin, Einstein and Smoluchowski, extended here to the case where several Brownian particles are harmonically coupled.

The formalism itself dates back to the work of Paul E. Rouse, who first proposed a simple yet powerful model for the dynamics of polymer chains. The role of the computer in this story is 3--fold: to generate the noise that drives the dynamics, to integrate the resulting equations of motion and finally to offer us the chance to observe, to interpret and perhaps to glimpse a bit of order within the fluctuations.

For this, I chose to work in Common Lisp. That is a language whose core is as old as the Rouse model itself, and one that encourages the same kind of dialogue between abstraction and concreteness. It feels appropriate that model born from simplicity should be explored through a language built for expressing ideas. The rest of this report is an attempt to retrace that journey --- from equations to motion, from noise to structure.

= Theoretical aspects

The goal here is not to recount the entiry history of Brownian theory, but to offer a few insights into it. Bertrand Duplantier gives a far more exhaustive and elegant account in his _Séminaire Poincaré_ lecture (2005) @duplantier_mouvement_2005  --- one can only recommend reading the transcript.

It all began, quite innocently, in 1827 when the botanist Robert Brown observed that pollen grains suspended in water exhibited an erratic and never-ending motion. The phenomenon was puzzling: it persisted even when the grains were no longer alive, ruling out any biological cause. Its physical origin would remain debated for nearly a century.

In 1905, Albert Einstein @einstein_investigations_nodate proposed a quantitative explanation. He showed that this irregular motion could be understood as the visible consequence of molecular collisions. Instead of following the particle itself, he described the probability of finding it at a certain position in time, using the diffusion equation.

A few months later, the physicist Marian Smoluchowski @smoluchowski_sur_1924 offered an alternative viewpoint. Where Einstein's approach was statistical, Smoluchowski's was kinetic and mechanical. He pictured the suspended particle as being constantly bombard by the molecules of the fluid, each collision imparting a tiny impulse. From this more intuitive picture, he recovered the same diffusion law, but with an emphasis on the discrete nature of the random kicks.

Finally, in 1908, Paul Langevin @langevin_sur_1908 brought synthesis and clarity. He wrote down directly the equation of motion for a Brownian particle --- developed in the next section.

Since then, the formalization of these stochastic equations has continued, notably through Itô calculus. The fact that such equations now stand at the core of modern financial models was, one suspects, motivation enough for mathematicians to polish their theory. Physicists, meanwhile, took another path. They introduced external potentials, coupled multiple particles and explored how collective fluctuations emerge from these stochastic laws. These efforts gave rise to techniques such as electrophoresis and, of course, to the Rouse model @rouse_theory_1953, which remains until this day an insightful description of polymer dynamics. The next section recalls the essence of Langevin's idea, an equation simple enough to fit in a single line, yet rich enough to describe a century of stochastic motion.


== Langevin equation<section:langevin>

Let us start with what is, perhaps, the master equation of all terrestrial beings: the time evolution of a body's acceleration is governed by the forces acting upon it,

$
    m partial_t bold(v) = sum_i bold(f)_i
$

Let this body be a small particle immersed in a viscous fluid. The surrounding medium exerts a resistance proportional to its velocity --- a simple friction term,

$
    m partial_t bold(v) = - gamma bold(v)
$

where $gamma$ is the viscous drag coefficient. This viscous coefficient is given by the Stokes' law, in the form $gamma = 6 pi eta R$ where $eta$ is the dynamic viscosity of the fluid and $R$ the typical dimension of our particle. The solution is straightforwardly,

$
    bold(v)(t) = bold(v)_0 e^(-gamma t \/m) 
$

The velocity decays exponentially and the motion dies away. The particle eventually comes to rest, yet under the microscope, that is not what we see. The motion never ceases, it persists. There must be another force at play. Langevin proposed to introduce such a random force $xi$ to the equations of motion, leading to

$
    m partial_t bold(v) = -gamma bold(v) + bold(xi)(t)
$<eq:langevin>

with the requirement that

$
    angle.l bold(xi)(t) angle.r = 0
$<eq:prop1_xi>


since the particle, though constantly shaken, achieves no net motion over time. But what is the strength of this force? Mathematically, since the average of $bold(xi)$ is zero, we must instead look at its variance. This can be understood in the light of the fluctuation-dissipation theorem, which states that the amplitude of the random fluctuations is directly tied to the strength of the dissipative term, such that,

$
    angle.l xi_alpha (t) xi_beta (t') angle.r = 2 gamma k_B T dot delta_(alpha beta)  dot delta(t - t')
$<eq:prop2_xi>

where greek letters denote coordinates of $bold(xi)$ and where $k_B$ is the Boltzmann constant and $T$ the temperature. With these properties, $bold(xi)$ is known as a Gaussian white noise. These considerations are modern#footnote[I would like to apologize for the lack of derivation here. I am familiar with the derivation of @eq:prop2_xi using the integrating factor --- I simply chose not to include it. None of the demonstrations I have come across seem to _physically_ justify the path they take, and perhaps out of a lack of technical skill on my part, it feels to me more like a matter of mathematics than of physical intuition. To the reader, sorry, again.] --- Langevin himself had no knowledge of them. For instance, the appearance of the $delta(t-t')$ implicitly enforces what we now call the Markov property: the random force acting on the particles at a given instant $t$ is independent of the forces that acted in the past, and likewise uncorrelated with those that will act in the future. Moreover, the mathematical nature of this noise, while perfectly acceptable from a physicist's point of view, deserves a closer look. We will return to this question when discussing the numerical resolution of the Langevin equation.


== Rouse model<section:rouse>

To move on to the Rouse model, we must now consider a collection of $N$ identical particles, called beads, whose dynamics each follow the @eq:langevin of Langevin. This time, however, we introduce an additional coupling term --- a harmonic interaction $U$ that links neighboring particles together. Thus, for the $i$th particule, the equation of motion takes the general form,

$
    m partial_t bold(v)_i = - bold(nabla)_bold(x)_i U - gamma bold(v)_i + bold(xi)_i
$

In practice, the bead mass $m$ is tiny (typically $m ~ 10^(-25)$ kg) compared with the viscous scale set by $gamma$. The momentum-relaxation time $tau_m = m \/ gamma$ is therefore extremely short, much smaller than the spring and diffusive times that governs the dynamics. Since the thermal noise amplitude scales as $sqrt(2 gamma k_B T)$ (fluctuation-dissipation), and the harmonic coupling captures bond forces, the inertial term $m_i partial_t bold(v)_i$ can be safely neglected and the bead enters the overdamped regime,

$
    gamma bold(v)_i &= - bold(nabla)_bold(x)_i U +  bold(xi)_i \

<=> gamma partial_t bold(x)_i &= - bold(nabla)_bold(x)_i U + bold(xi)_i
$

once rewritten using position instead of velocity. Then, for a chain of $N$ beads (indices $i=0,...,N-1$) with the harmonic potential,
$
    U = k/2 sum_(i=0)^(N-2) ||x_(i+1)- x_i ||^2
$

where $k$ is the spring constant, it follows that the system

$
    gamma partial_t bold(x)_0 &= k (x_1 - x_0) + bold(xi)_0 \
    gamma partial_t bold(x)_i &= k (x_(i+1) - 2x_i + x_(i-1)) + bold(xi)_i \
    gamma partial_t bold(x)_(N-1) &= k (x_(N-2) - x_(N-1)) + bold(xi)_(N-1)\
    angle.l bold(xi)_i angle.r &= 0\
    angle.l xi_i_alpha (t) xi_j_beta (t') angle.r &= 2 gamma k_B T dot delta_(i j) dot delta_(alpha beta)  dot delta(t - t')
$<eq:rouse>

forms our complete set of equations to solve for the Rouse model in the overdamped limit. To solve this coupled system, Rouse proposed to introduce a set of orthogonal coordinates --- the Rouse modes --- that diagonalize the harmonic coupling between beads. For sake of clarity, we will follow the derivation proposed in @briels_theory_2007. These solutions were of the form,

$
    bold(X)_p (t) = 1/N sum_(i=0)^(N-1) bold(x)_i (t) cos[(p pi)/N (i + 1/2)]
$<eq:mode_rouse>

Each of these modes describes a collective motion of the chain, analogous to the normal modes of a vibrating string, but damped and driven by noise. The lowest modes (small $p$) govern the large-scale motions of the chain, while higher modes describe faster, local fluctuations. The mode $0$ corresponds to the diffusion of the center of mass,

$
    bold(X)_0 (t) = 1/N sum_(i=0)^(N-1) bold(x)_i (t)
$

We can now reverse @eq:mode_rouse, giving

$
    bold(x)_i (t) = bold(X)_0 (t) + 2 sum_(p=1)^(N-1) bold(X)_p (t) cos[(p pi)/N (i + 1/2)]
$<eq:mode_rouse_indiv>


which can be injected into @eq:rouse. We want to emphasize that, by doing so, the equations now decouple and we are left with something that goes as,

$
    gamma partial_t bold(X)_p = - k_p bold(X)_p + bold(Xi)_p 
$<eq:langevin_mode>

where $k_p = 4k sin^2 [p pi\/2N ]$ represents an effective spring constant for the mode $p$ and $bold(Xi)$ the corresponding white noise. The formal solution can be written as

$
    bold(X)_p (t) = bold(X)_p (0) e^(-t\/tau_P) + cal(F) [bold(Xi)_p]
$

with $tau_p = gamma \/ k_p$  being the characteristic relaxation time of mode $p$ and $cal(F)$ a functional of $bold(Xi)_p$ representing the fluctuations. The Rouse modes do not oscillate; their amplitude decay exponentially, independently, with a finite lifetime. We will attempt to measure these later on.

== Observables<section:observables>

Before turning to simulations, let us briefly recall which quantities can be observed. Most of the observables discussed below are not specific to the Rouse model. They are rather standard quantities used to characterize the structure and dynamics of polymer chains --- both in theory and simulation. Most of the quantities of interest are derived from @rubinstein_polymer_2003.

The story begins with a simple observation: if the chain vibrates, stirs, and folds, its center of mass must itself diffuse within the volume. But how much does it move, and how does it move? The answer lies in @section:rouse where we mentioned that the mode $p = 0$ corresponds to the diffusion of the center of mass of the chain. Then, according to @eq:langevin_mode,

$
    partial_t bold(X)_0 = 1/gamma bold(Xi)_0 space #text[ with ] space bold(Xi)_0 = 1/N sum_(i=0)^(N-1) bold(xi)_i
$

Since the individual random forces $bold(xi)_i$ are uncorrelated, their sum produces an effective noise of reduced strength,

$
    lr(angle.l Xi_0_alpha (t) Xi_0_beta (t') angle.r) = (2 gamma k_B T)/N dot delta_(alpha beta) dot delta(t-t') 
$

The formal solution is,

$
    bold(X)_0(t) = bold(X)_0(0) + 1/gamma integral_0^t bold(Xi)_0 (s) dif s
$

We can derive a quantitative feature from there, in the name of mean-squared displacement (_abbrev_. MSD), by rewriting the previous relation and square it on both sides,

$
    (bold(X_0) (t) - bold(X)_0 (0))^2  = 1/gamma^2 integral_0^(t) integral_0^t bold(Xi)_0 (s) bold(Xi)_0 (s') dif s dif s'
$

Averaging quantities out,

$
    lr(angle.l (bold(X_0) (t) - bold(X)_0 (0))^2  angle.r) &= 1/gamma^2 integral_0^t integral_0^t lr(angle.l bold(Xi)_0 (s) bold(Xi)_0 (s) angle.r) dif s dif s' \
        &= 1/gamma^2 integral_0^t integral_0^t (6 gamma k_B T)/N dot delta(s-s')  dif s dif s'
$


which simplifies into

$
    lr(angle.l (bold(X_0) (t) - bold(X)_0 (0))^2  angle.r) &= 6 (k_B T )/(gamma N) t
$<eq:diffusion>

This linear dependence of the mean-squared displacement on time is the signature of Brownian motion. By comparison with the Einstein relation $angle.l (Delta R)^2 angle.r = 6 D t$ we can identify the diffusion coefficient of the center of mass as $D = k_B T \/ gamma N$. It follows that the center of mass of the polymer diffuses $N$ times slower than for a single bead, as if the entire chain behaved as a single particle with an effective friction $gamma N$.

Regarding the individual beads, the situation is more complex. Because each bead is coupled to its neighbours through the harmonic potential, its motion cannot be derived as straightforwardly as for the center of mass. In the literature, two dynamical regimes are usually distinguished when analyzing the MSD: in addition to the diffusive regime --- governed by the interactions with the solvent, and  scaling as $t^1$ ---  there is the subdiffusive regime that is mainly constrained by the harmonic potential, scaling as $t^(1\/2)$.


Between these two limits, one may expect intermediate regimes where both harmonic constraints and thermal fluctuations act on comparable timescales. Moreover, at very short times, a ballistic regime may be observed: before the beads have thermalized and start to feel the effects of the coupling potential or the solvent, their motion is nearly free, resulting in an MSD scaling as $t^2$. This inertial regime is generally too short-lived to be captured in overdamped simulations.

After discussing global motions, we can now turn to the linear structure of the system. One of the first quantities of interest is the linear dimension of the chain. At equilibrium, the total length $cal(l)$ can be roughly estimated as $cal(l) = N b$ where $b$ is the typical bond length (or spring equilibrium length) between two consecutive beads.

However, this estimate is not particularly informative, since the chain tends to fold and coil like a ball of yarn --- making it difficult to relate $cal(l)$ to the actual space occupied by the chain. A more meaningful is the end-to-end distance, $R_e^2$, defined as the quadratic distance between the two ends of the chain. As shown in @rubinstein_polymer_2003, its mean value for a Rouse-like chain scales as,

$
    angle.l bold(R)_e^2 angle.r = N b^2 
$<eq:endtoend>

Another closely related measure is the radius of gyration $R_g^2$ which represents the squared distance between the beads and the center of mass. Again, for a Rouse-like chain, these two quantities are related through,

$
    angle.l bold(R)_e^2 angle.r = 6 angle.l R_g^2 angle.r
$

Together, $R_e$ and $R_g$ provides complementary information: the first characterizes the extension of the chain, while the second captures how the mass is distributed within the space. The contribution of modes to $R_e$ can be computed,

$
    bold(R)_e &= bold(x)_(N-1) - bold(x)_0
        &= 2 sum_(p=1)^(N-1) bold(X)_p [(-1)^p - 1 ] cos((p pi) /(2 N))
$

The observation is 2--fold. First, the amplitude of $bold(R)_e$ depends on how the different modes $bold(X)_p$ are populated. Second, because each mode $bold(X)_p$ relaxes exponentially with its own characteristic time $tau_p$, the end-to-end distance is also expected to decay in time, reflecting the chain's tendancy to retract and fold as internal tensions are relaxed.

Consequently, the population of the Rouse modes becomes a key quantity to investigate. A natural way to characterize it is to look at how each mode contributes to the energy of the chain. As discussed in the previous section, each mode behaves as an independent harmonic oscillator with an effective spring constant $k_p$. Therefore, applying the equipartition theorem yields a relationship between the mode index $p$ and its average amplitude, since,

$
    1/2 k_p lr(angle.l |bold(X)_p|^2 angle.r) &= 3/2 k_B T\

=> lr(angle.l |bold(X)_p|^2 angle.r) &= 3 k_B/k_p T
$

Recall that $k_p = 4k sin^2 [p pi\/2N ]$. When the chain is long enough, one can go a bit further for the slowest modes $p << N$, since $k_p approx 4k (p pi\/2N )^2$. From there, it follows that,

$
    lr(angle.l |bold(X)_p|^2 angle.r) prop  p^(-2)
$<eq:population>

This inverse-square dependence implies that the slowest modes contribute the most to the overall conformational dynamics. My last words are to give a practical way to measure the relaxation times $tau_p$. Starting from the formal solution of @eq:langevin_mode,


$
    bold(X)_p (t) = bold(X)_p (0) e^(-t\/tau_P) +  cal(F) [bold(Xi)_p]
$

one can compute the time autocorrelation function,

$
    angle.l bold(X)_p (t) bold(X)_p (0) angle.r &= angle.l |bold(X)_p (0)|^2 angle.r e^(-t\/tau_P) + angle.l (bold(X)_p (0) times cal(F) [bold(Xi)_p]) angle.r\
        &= angle.l |bold(X)_p (0)|^2 angle.r e^(-t\/tau_P) + cal(F)' [ angle.l bold(X)_p (0) bold(Xi)_p  angle.r ]
$


Physically, this cross-term vanishes because the random force $bold(Xi)_p$ represents new thermal kicks acting at time $t$, which have no memory --- due to the Markov property aforementioned --- of the configuration at time $t = 0$. The initial $bold(X)_p (0)$ results from past noise realizations, while $bold(Xi)_p$ reflects independent, instantaneous fluctuations of the solvent. As a result, their correlation averages to zero. Finally,

$
    (angle.l bold(X)_p (t) bold(X)_p (0) angle.r)/(angle.l |bold(X)_p (0)|^2 angle.r) = e^(-t\/tau_P)
$<eq:relaxation_time>

This relation provides a direct way to extract the relaxation times $tau_p$ from simulations, by fitting the decay of this autocorrelation function. This closes our theoretical overview of the Rouse model.

= Implementation

The idea of Lisp was born with John McCarthy in 1958 @mccarthy_recursive_1960, five years after the Rouse model, and it has survived ever since through many dialects --- Emacs Lisp, Racket, Clojure and Common Lisp among the best known today. At its core lay a remarkably elegant insight: that a program could be nothing more than a structured collection of symbols, and that the very syntax of computation could be represented as a list.

And once one accepts that a program is nothing but a list, an inevitable idea follows. McCarthy realized that one could write functions --- _macros_ --- that take lists as input and return new lists as output. In other words, programs capable of writing programs. This reflexive capacity gave Lisp its extraordinary flexibility: the language could adapt to the problem, reshape itself to fit a domain, or even extend its own syntax. It is often summarized by the motto: _code is data_.

Beyond its syntax, Lisp introduced another quiet revolution: the idea of interaction. The READ-EVAL-PRINT-LOOP (REPL) turned programming from a static act of compilation into a genuine dialogue. One can feed expressions to the machine, observe their behavior, modify them and start again --- all in real time. This immediate feedback transforms the way models are built. Everything emerges iteratively, as the programmer is feeding thoughts into the process.

In that sense, modern environments such as Jupyter notebooks or interactive shells in Python, Julia or JavaScripts are only distant echoes of this original idea. Likewise, the macros proposed in C++ or Rust are little more than preprocessor tricks. They lack reflection. They lack semantic coherence. In Lisp, macros are not text substitutions; they are meta-programs, capable of reshaping the language itself from within. Deprived of Lisp's unified syntax and symbolism, they borrow some of its features while remaining bound to the rigidity of C-like grammars, condemning themselves to be forever less expressive.

Here, I would like to give a glimpse of what a molecular dynamics tool written in Lisp could look like. Those who have used some of the popular solutions (GROMACS, LAMMPS, ...) might well regret that things did not take such a path. One could perhaps mention OpenMM, which indeed is on the right track but, alas, has fallen in love with Python. Before giving a taste of it, however, we must return for a moment to the equations.

== Euler-Maruyama

We must now address this white noise term, the random force. As mentioned earlier, for physicists this term feels natural; Langevin himself introduced it explicitly in his model. It has the dimension of a force and, one imagines it as representing, as each instant, the effect of random molecular kicks, with the statistical properties discussed in @section:langevin.

Yet, rigor demands that we understand this noise in a probabilistic sense. Mathematicians have formalized it through the notion of Wiener processes, which provide a convenient way (among others) to encode randomness in continuous time. Formally, one writes,

$
    bold(xi)  &= sqrt(2 gamma k_B T) dot (dif bold(W)_t) / (dif t)\
    angle.l bold(W)_t angle.r  &= 0\
    angle.l W_(t_alpha)^2 angle.r &= t
$<eq:def_random>

From there, one can simply rewrites the Langevin equation in its differential form,

$
    dif bold(x) = - 1/gamma bold(nabla) U dif t + 1/gamma bold(xi) dif t
$

which, by virtue of @eq:def_random, can be stated as

$
    dif bold(x) = - 1/gamma bold(nabla) U dif t + sqrt((2 k_B T)/gamma ) dif bold(W)_t
$

The term $dif bold(W)_t$ expresses a small increment in the Wiener process. To implement this Wiener process numerically, one can simply draw a random number $eta_alpha$ from a standard normale distribution $cal(N) (0,1)$, and write that the Wiener increment is given by $dif W_t_alpha = a eta_alpha$ with $a$ a proportionality constant. We can check that this construction satisfies the expected statistical properties.

For the mean, the result is immediate, since $angle.l eta_alpha angle.r = 0$, hence $angle.l dif W_t_alpha angle.r = 0$. For the mean-squared displacement, one has,

$
    angle.l dif W_(t_alpha) ^2 angle.r = a^2 angle.l eta_alpha^2 angle.r = a^2
$

Comparing with the theoretical definition immediately lands $a = sqrt(dif t)$. Thus, in practice, the discrete form of the Wiener process reads,

$
    Delta W_t_alpha = eta_alpha sqrt(Delta t), space space eta_alpha ~ cal(N)(0,1)
$<eq:wiener>


By discretizing the Langevin equation,

$
    Delta bold(x) approx bold(x)(t + Delta t) - bold(x)(t) &= - 1/gamma bold(nabla) U Delta t + sqrt((2 k_B T)/gamma) dot bold(eta) dot sqrt(Delta t)
$

we obtain the *Euler-Maruyama* scheme,

$
    bold(x)(t + Delta t) = bold(x)(t) - 1/gamma bold(nabla) U Delta t + sqrt((2 k_B T)/gamma) dot bold(eta) dot sqrt(Delta t) + O(Delta t)
$<eq:maruyama>

== Common Lisp & Architecture

When one starts implementing a molecular dynamics engine, the first practical question is not only the algorithm, but the environment in which it will live. Most existing tools provide remarkable numerical engines, yet they remain monolithic or even opaque. They offer efficiency, but they lack intimacy. They are computationally powerful but rigid. Launching a simulation can feel almost as exhausting as doing labwork.

But to me, a molecular dynamics engine should be seen as a tool for _in silico_ experimentation. Let us recall what we truly do: we integrate equations derived from models, and we test what our assumptions imply. What we need, then, are tools suited to the pace of theoretical reasoning, _i.e._ to the act of erasing, rewriting and simplifying as one does on a blackboard.

This project was born precisely from that absence: a lack of flexibility, of agility, of an environment that feels alive. Common Lisp, with its symbolic nature and living runtime, offered exactly that. It allows a style of development that is both exploratory and precise, where one can expression physical ideas directly in code and see them evolve interactively.

From this starting point, the implementation was designed to remain modular and transparent. Each main components could be tested or replaced independantly. The codebase is divided into six main packages: topology, control, test, viewer, simulation & scripts. Each component is relatively independent and could we rewritten or extended without difficulty --- a design choice meant to favor modularity and long-term maintainability. Let us briefly walk through some of them,

- The *topology* module is in charge of building the chain. It relies on two simple classes: `chain`, which contains a list of beads, and `bead` which holds the physical information of each brownian particle --- its position, mass and possibly velocity. Around these structures live a few methods to access positions, extract properties or compute derived quantities (center of mass, radius of gyration, ...). But perhaps more importantly, a small set of macros lets us write concise and expressive topology definitions. For instance, the following expression, ```lisp
(top:make-linear-chain 20 :along :y :spaced-by 1d-9)
``` will generate a chain of 20 beads, aligned along the y--axis, with a spacing of 1nm between each bead at time zero.

- On the other side, the *simulation* module contains the core simulation logic. It defines what a simulation `state` is --- a combination of physical parameters ($gamma, T, Delta t$, ...) and a polymer `chain` at a given time. A simulation is then represented as a class holding a `timeline`, _i.e._ a list of states stored in a LIFO structure, together with a `cursor` pointing to the current state.\ \ This design is quite distinctive: it allows one to move `backward`or `forward` in time at will, using dedicated methods, to modify either the physical parameters or the chain on the fly, and to recompute trajectories instantly from any previous state. Running replicas at different temperatures is therefore as simple as,\ \ ```lisp
(defvar *sim*
  (sim:make-simulation
    :chain (top:make-linear-chain 20 :along :y :spaced-by 1d-9)
    :temperature 273.15 :gamma 1d-11 :k 1.2d-2 :dt 1d-12))

(sim:propagate *sim* :steps 1000) ;; thermal equilibrium reached after 1ns

(loop for temperature in '(273.15 280 290 300)
      do (let ((current-state (sim:current-state)))
            ;; changing the temperature
            (setf (sim:state-temperature current-state) temperature)
            ;; computing new solutions
            (sim:propagate *sim* :steps 10000 :save-every 100)
            ;; exporting solutions to a file
            (sim:export-simulation *sim* (format nil "~a" temperature))
            (sim:backward *sim* :steps 1000)))
``` I challenge the reader to do it with such ease in GROMACS.
- Finally, the *viewer* module follows the same philosophy, allowing one to visualize the simulation as it is being shaped by thought. Its implementation remains minimal. Due to time contraints, only a single view has been developed so far: the so-called `ortho-view`, illustrated in @fig:viewer. This view displays the motion of the chain projected along the cartesian axes ($plus.minus X, plus.minus Y, plus.minus Z)$, while removing the diffusion of the center of mass so that one can focus purely on the internal dynamics. It also provides a few convient interactions; for instance, the $arrow.l$ and $arrow.r$ keys allow one to navigate backward and forward in time through the timeline. To launch it, ```lisp
(view:view *sim* :mode :ortho-view)
```


Most of these functions have been verified through integration tests, but more importantly through unit testing. While the code coverage is not exhaustive, the tests target the most critical components of the system and ensure that each module behaves as intended.

A final note about Common Lisp: although born in the 1980s and only slighty updated since, its ecosystem for numerical computation remains surprisingly limited. For instance, implementing the Euler-Maruyama scheme requires drawing samples from a normal distribution, yet Common Lisp --- to the best of my knowledge --- provides no native generator for $cal(N)(0,1)$. I therefore reimplemented the Box-Muller transform myself.

#figure(
    image("rsrc/illu-viewer.png", width: 60%),
    caption: [Visual feedback during simulation, as displayed by the `ortho-view` mode.]
)<fig:viewer>

Finally, the *scripts* package contains a set of Python scripts used for figure generation and physical analysis, bridging the Lisp simulation data with more conventional visualization tools.

== Physical validation

We would not want to spoil the reader too early about the behavior of the Rouse modes! For this reason --- and because our integrator conveniently allows it --- we shall start in a regime that is actually poorly captured by the Rouse model: that of a short chain. We will focus mainly on its diffusive properties, which provide a simple yet effective way to test the physical consistency of our simulation engine, and some structural behavior.

We will work at constant $N,T$ and set the temperature to $T=273.15$K. The chain length is kept short --- $N = 20$ --- with a bead mass of order $m ~10^(-25)$. The viscous coefficient is estimated from Stokes' law,

$
    gamma = 6 pi eta^star R
$

with $eta^star$ the viscosity of water and $R$ a typical bead radius. At this temperature and for a typical radius $R ~ 10 angstrom$, $gamma$ is of order $10^(-11)$ kg/s. Since bonds are harmonic, equipartition sets the spring constant through

$ 1/2 k b^2 = 1/2 k_B T space space => space k = (k_B T)/b^2
$

With $b = 0.56$ nm, this gives $k ~ 1.2 times 10^(-2)$ N/m. The momentum-relaxation time is thus $tau_m = m\/gamma = 10^(-14)$s. With a timestep $Delta t =  0.1$ ps, we are thus safely in the overdamped regime. It is with these parameters that we launch the following code for 150 ns,

```lisp
(in-package #:rouse)

(defvar *sim-test*
  (sim:make-simulation
   :chain (top:make-linear-chain 20 :along :y :spaced-by 1d-9 :mass 1d-25)
   :temperature 273.15 :gamma 1d-11 :k 1.2d-2 :dt 1d-13))

(sim:propagate *sim-test* :steps 1500000 :save-every 1500)
(sim:export-simulation *sim-test* "150ns")
```

#figure(
    image("rsrc/test-com-brownian.png", width: 70%),
    caption: [Trajectory of the polymer center of mass over 150 ns.\ The motion is purely diffusive, as expected for a Brownian particle in the overdamped regime.
]
)<fig:test-com-brownian>



Focusing now on the physics of the center of mass, we first note that the COM exhibits a clear Brownian motion, diffusing freely in space as shown in @fig:test-com-brownian. To quantify this behavior, we compute its mean-squared displacement and fit the linear region according to @eq:diffusion. The result is shown in @fig:test-com-r2 against time lags, and the corresponding diffusion constants are,

$
    D_#text[com]^#text[fit] = 1.71 times 10^(-12)  space #text[m]^2#text[/s] space space ; space space   D_#text[com]^#text[th] = 1.88 times 10^(-12) space  #text[m]^2#text[/s]
$


The integrator conserves the expected diffusive scaling law, within 10% accuracy, which is consistent regarding the short duration of the simulation. At large time lags, the MSD becomes noisier due to the reduced number of samples available for averaging, a purely statistical effect that does not affect the physical interpretation.



#figure(
    image("rsrc/test-com-r2.png", width: 60%),
    caption: [Mean-squared displacement of the center of mass as function of time lags.\
        The linear dependence confirms diffusive behavior. The fitted slope provides the diffusion coefficient $D_#text[com]^#text[fit]$.]
)<fig:test-com-r2>


If we now focus on the diffusion of a single bead, we should be able to observe the two characteristic regimes discussed in @section:observables: a subdiffusive regime and a diffusive one. @fig:test-r2 (left) shows the MSD of bead 3, and @fig:test-r2 (right) extends this to several beads along the chain to remove any ambiguity that the behavior might be specific to one position.

What we observe is consistent with the literature: at short times, a brief diffusive regime --- possibly the traces of a ballistic phase --- is followed by a subdiffusive regime where the bead is transiently constrained by its neighbors. At longer times, the dynamics cross over back to diffusion as the bead ultimately follows the global motion of the chain. @fig:test-r2 (right) further shows that beads located near the center of the chain (_e.g._, the 8th) exhibit slightly smaller amplitude in the long run than those at the ends, which is likely a signature of Rouse-like dynamics --- a point that will be further explored in the next section.


#figure(
     grid(
         columns:(50%,50%),
         image("rsrc/test-one-r2.png", width: 100%),
         image("rsrc/test-some-r2.png", width: 100%),
     ),

    caption: [(*Left*) Mean-squared displacement of the bead 3.\
        At short times, a brief diffusive regime is followed by a subdiffusive one, before recovering diffusion at long times. The dashed lines indicate the expected scaling laws.\ 
        (*Right*)  Mean-squared displacement of some beads (3rd, 8th and 15th) as a function of time lags.\ All beads exhibit a short-time diffusive regime followed by a subdiffusive one, with end beads showing slightly larger amplitudes due to reduced connectivity.]
)<fig:test-r2>

To isolate discretization effects, we reran the simulations with the same random seed but a time step ten times larger. The MSD of the center of mass and of single beads retained the same type of slopes and fitted diffusion constant remained within statistical uncertainty. This indicates that, in our parameter range, the implementation of the Euler-Maruyama scheme is not introducing a systematic bias, provided that the chosen $Delta t$ remains coherent with overdamped timescales.

#figure(
    image("rsrc/test-rg2.png", width: 60%),
    caption: [The radius of gyration $R_g^2$ (blue) progressively relaxes toward the theoretical ideal-chain value $N b^2 \/ 6$ (dotted line). The convergence confirms that the chain has reached its equilibrium conformational size.]
)<fig:test-rg2>


Finally, we verified that the structural properties of the chain were also correctly reproduced. @fig:test-rg2 shows the time evolution of the radius of gyration $R_g^2$. Starting from a stretched initial configuration, the chain rapidly collapses and equilibrates around the theoretical value $N b^2\/6$, as expected. Taken together with the diffusive results, this agreement indicates that our simulation engine may well capture both the dynamical and structural aspects of the physics.

= Results & Physical discussion

Perhaps, at first glance, the reader might find this model somewhat toy-like. Yet, it is worth insisting on what it truly represents. As mentioned earlier, the Rouse model is nothing more than the coupling of Brownian particles, which, on paper, translates into the study of the evolution of a linear structure freely moving in a solvent.

Such structures are ubiquitous. They form the very basis of polymer physics --- and, more importantly, of biological polymers. They are your proteins. They are your membrane receptors for drugs. They are your DNA and RNA. Taken together, they assemble into larger complexes --- your enzymes. What the Rouse model ultimately proposes it not merely a toy picture of coupled beads, but rather an effective coarse-graining of the flexible chains that make up life itself.

This kind of coarse-graining is standard in molecular simulations. As the theoretical part (@section:rouse) showed, these objects are governed by slow, collective timescales --- often far beyond the reach of atomistic simulations. On a typical mesocenter, one may reach hundreds of nanoseconds, or the microsecond scale for the most ambitious runs, in all-atoms. Yet these remain insufficient to capture the long-time dynamics of chains. Hence, one relies on such simplified models --- not as a compromise, but as a bridge toward the physical essence of the problem.

In this perspective, the Rouse model can in fact reveal something quite interesting. The whole construction --- from the equations to the simulation --- should be understood under this enlightenment. To emphaze this, we now place ourselves in a setup closer to a biological environment. We set the temperature to $T = 310$K, corresponding to the human body temperature, and consider a longer chain with $N = 80$ beads. Following the same procedure as in the previous section, we derive the physical parameters as,

$
    gamma = 10^(-11) #text[kg/s] &, space space m = 10^(-25) #text[kg],\
    k = 1.2 times 10^(-2) #text[N/m]&, space space Delta t = 1 #text[ps]
$

The chain spacing is set to $b = 0.56$ nm. We then propagate the system using the Euler-Maruyama integrator for a total simulated time of $20$ µs.

== Diffusion, Structure

We start by checking again the global dynamics of the chain by looking at its COM and a few structural observables. The trajectory of the COM, shown in @fig:long-com-brownian, exhibits the same Brownian character as previously observed, confirming that the system remains well in the overdamped regime.

To quantify this behavior, we again computed the MSD of the COM, displayed in @fig:long-com-r2. Thanks to the longer simulation time and finer sampling, the linear diffusive regime is now clearly identified, allowing for a more reliable fit of the slope. From this fit, we obtain,

$
    D_#text[com]^#text[fit] = 5.70 times 10^(-12) space #text[m]^2#text[/s] space space ; space space   D_#text[com]^#text[th] = 5.35 times 10^(-12) space  #text[m]^2#text[/s]
$

The agreement, within roughly 6.5%, confirms once again that the simulation engine correctly reproduces the expected diffusive dynamics, even for longer chains and timescales.



#figure(
    image("rsrc/long-com-brownian.png", width: 60%),
    caption: [Trajectory of the polymer center of mass over 20 µs.\
The motion is purely Brownian, consistent with the overdamped Langevin regime and confirming the correct implementation of the integrator.]
)<fig:long-com-brownian>




#figure(
    image("rsrc/long-com-r2.png", width: 60%),
    caption: [Mean-squared displacement of the center of mass as a function of time lags. \ The MSD grows linearly with time, characteristic of diffusive motion.
        The fitted slope gives a diffusion coefficient $ D_#text[com]^#text[fit] = 5.70 times 10^(-12) space #text[m]^2#text[/s]$, in excellent agreement (6.5 %) with the theoretical value $D_#text[com]^#text[th] = 5.35 times 10^(-12) space  #text[m]^2#text[/s]$.]
)<fig:long-com-r2>

In @fig:long-re2-rg2, we report the time evolution of the end-to-end distance and the radius of gyration, respectively. Both quantities show a relaxation from the stretched initial condition and fluctuate around their expected theoretical values. Averaging over the trajectory gives,

$
    angle.l R_g^2 angle.r = 1.62 times 10^(-17) space #text[m]^2 space space ; space space angle.l R_g^2 angle.r_#text[th] = 1.43 times 10^(-17) space #text[m]^2

$



$
    angle.l R_e^2 angle.r = 9.36 times 10^(-17) space #text[m]^2 space space ; space space angle.l R_e^2 angle.r_#text[th] = 8.56 times 10^(-17) space #text[m]^2
$

These values agrees within $~7$%, confirming that the equilibrium structure is correctly captured. The ratio $R_e^2 \/ R_g^2$, shown in @fig:long-re2rg2, oscillates around 6, as theoretically predicted.


#figure(
    grid(
        columns:(50%,50%),
        image("rsrc/long-re2.png", width: 100%),
        image("rsrc/long-rg2.png", width: 100%),
    ),
    caption: [Time evolution of the (*Left*) end-to-end distance (*Right*) radius of gyration.\
        Starting from an initially stretched configuration, the chain quickly relaxes and fluctuates around its theoretical equilibrium value. \ Both observables display comparable dynamical fluctuations, though end-to-end distance exhibits stronger variations due to the larger influence of the end beads.]    
)<fig:long-re2-rg2>



#figure(
    image("rsrc/long-re2rg2.png", width: 60%),
    caption: [The ratio $R_e^2 \/ R_g^2$ as a function of time.]
)<fig:long-re2rg2>


== Rouse dynamics

Now comes the time to watch the theoretical derivations of the Rouse model come to life. A first remark: we should not expect to capture the fastest modes, as our time step may be limiting, and more importantly, the simulation is not recorded at every frame --- otherwise, trajectories would quickly fill our disks. But what do Rouse modes actually look like in practice ? @fig:long-rxp shows a few of them projected on the $x$--cordinate. The mode $p =0$ (blue) corresponds to the diffusive motion of the center of mass, while higher modes represent internal fluctuations of increasing frequency and decreasing amplitude.


#figure(
    image("rsrc/long-rxp.png", width: 60%),
    caption: [Time evolution of the first Rouse modes along the x-axis.\
        The slowest modes display large-scale motion of the chain, while higher-order modes correspond to faster, small-scale internal fluctuations that average out over time.]
)<fig:long-rxp>


To measure the relaxation times of these modes, we follow the procedure outlined in @section:observables, by computing the autocorrelation function of each mode after projecting the trajectories onto the corresponding Rouse basis. @fig:long-autocorr-fit on left shows several of these normalized autocorrelation functions, while on right, the figure presents a typical exponential fit --- here for mode $p=1$. We chose to plot these correlations only over the first $2$ µs, since --- as shown in @fig:long-autocorr-fit and in @fig:long-taus --- the higher the $p$, the faster it decays. Given that the mode $p = 1$ relaxes on a timescale of about 0.6 µs, the signal beyond that is just noise.

Physically, these results show that different modes contribute to the chain dynamics on distinct spatial and temporal scales. Low--$p$ modes represent large-scale, coherent motions of the whole chain and therefore relax slowly. Higher--$p$ modes correspond to short-wavelength internal fluctuations, involving only neighboring segments, which decay much faster. The sharp decrease in relaxation times with increasing $p$ thus reflects a sort of hierarchy --- a hierarchy that is mirrored in the distributed of mode amplitudes. In fact, the mean population of each mode is expected to decrease as $p^(-2)$, reflecting the progressively smaller contribution of higher-order modes to the overal conformational motion.


We compute these mode populations from the simulation trajectories and compared them with the theoretical scaling (see @fig:long-pop). While the overall decay follows the expected trend, we observe a clear deviation at large $p$. We believe this deviation mainly reflects finite-size effects, as well as the limited temporal resolution and frame sampling of our simulation --- put differently, high--$p$ modes fluctuate too fast to be fully captured with our parameters.


#figure(
    grid(
        columns: (50%,50%),
        image("rsrc/long-autocorr.png", width: 100%),
        image("rsrc/long-fit-p1.png", width: 100%),
    ),
    caption: [(*Left*) Normalized autocorrelation functions of the first Rouse modes ($p=1$–$4$). Higher-order modes decay faster.\
        (*Right*) Normalized autocorrelation function of the Rouse mode $p = 1$.\ The exponential decay (black line) provides the relaxation time $tau_1 = 0.62$ µs.]
)<fig:long-autocorr-fit>



Beyond these numerical details, it is worth taking a step back to think about what these mode populations --- and their dynamics --- actually mean. At first glance, it is clearly the slow modes that dominate the average behavior of the chain: they hold both the largest amplitudes and the longest relaxation times, so they carry most of the energy and the collective motion. The fast modes, by contrast, decay quickly and contribute only weakly to global averages --- they are almost higher-order corrections in that sense. Still, it feels wrong to simply dismiss them.


#figure(
    image("rsrc/long-taus.png", width: 60%),
    caption: [Relaxation times $tau_p$ of Rouse  modes (1-10) as a function of mode index $p$.\
        The relaxation time decreases rapidly with $p$, indicating that higher modes relax much faster and contribute mainly at short times.]
)<fig:long-taus>

In a biological setting, say in the motion of membrane receptors, these fast, high-frequency fluctuations might be the ones that matter most for binding dynamics. They are small, subtle, but they are precisely where the system can react --- where it is sensitive enough to tiny conformational changes or external cues. Put differently, and if we go back to the theoretical section, we had introduced the idea of an effective spring constant $k_p$ associated with each Rouse mode. What this really means is that the low--$p$ modes correspond to very stiff effective springs: they strongly couple the motion of distant beads, enforcing large-scale coherence in the chain's dynamics. At the other end of the spectrum, in the weakly coupled regime of large $p$, the situation is inverted --- these modes make the beads much more sensitive to their local environment, allowing them to respond to small perturbations. These observations close our exploration of the Rouse model and set the stage for the concluding remarks.

#figure(
    image("rsrc/long-pop.png", width: 60%),
    caption: [Average population of the Rouse modes, $angle.l |bold(X)_p|^2 angle.r$, as a function of mode index p.]
)<fig:long-pop>

= Conclusions

This work was an attempt to bring the Rouse model to life --- not as an abstract theory, but as a tangible object that one can simulation, explore and shape. Starting from Langevin description of coupled Brownian particles, we derived the chain dynamics, implemented the Euler-Maruyama scheme, and validated the physical consistency of the results through diffusion, structural and relaxation analyses. Altogether, the simulations reproduced the expected scaling behaviors and provided a coherent picture of Rouse dynamics.


Along the way, this project also offered an introduction to the vocabulary of stochastic differential equations and, more broadly, to some of the key concepts of mesoscopic physics. It is a field that fascinates me, even though it is often a demanding one --- connecting two cliffs that stand on opposite sides is never easy.

From a physical point of view, the Rouse model has since been extended in many ways --- most notably by the Zimm model, which accounts more accurately for hydrodynamic interactions and often provides a closer description of real polymer behavior. Other advances, to my knowledge, have mostly followed this direction. I had initially planned to explore another one: that of constraints. I did not have the time, but the main idea was to constrain a few degrees of freedom --- as would naturally occur, for instance, in a membrane receptor whose backbone is partially trapped within a lipid slab, preventing from moving freely.

Still, I do not see this project as finished. Much of the effort was devoted to building a simulation engine that is both solid and modular --- and I hope I have shown that it can serve precisely the purpose to explore physics with flexibility and precision. What happens when I force inclusions into a membrane? What happens when I fix or release certain parts of the chain? I have the feeling that this Common Lisp approach is a natural companion to thought itself. And perhaps, through a few of these delicate tools, it will one day offer a few virtuous insights into the physics of living matter.



#pagebreak()
#bibliography(style:"american-physics-society", "rsrc/rouse.bib")
