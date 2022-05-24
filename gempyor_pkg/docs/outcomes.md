# Outcomes

Outcomes consists of a list of quantity of interested outputed by gempyor. A classical outcome is specified by:
- a _source_, either from some seir compartments or from another previously defined outcome
- a _probability_ of occurence w.r.t the source, if stochastic simulations are activated we perform binomial draws from the source, otherwise just as multiplicative proportions
- a _delay_ between the incidence in the source and the incidence in the outcome
- (not necessary) a _duration_: in this case a additional outcome is created (with name `$(OUTCOME_NAME)_curr` or the specified name with `name:` inside the duration). This new outcome stores the prevalence of this outcomes (e.g number of individuals currently in ICU).

An example config is provided below:

```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      incidH:                          # this is the name of the new outcome.
        source: incidI
        probability:
          value:
            distribution: fixed
            value: .2
        delay:
          value:
            distribution: fixed
            value: 14                 # As with the rest of gempyor, the unit is [days]
        duration:
          value:
            distribution: fixed
            value: 14
            name: hosp_curr               # custom prevalence compartment name goes here.
```

A other way of defining an outcome can be the sum of previously defined outcomes:
```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      # [...] define outcomes incidH_1dose & incidH_0dose 
      incidH_from_sum:                                              
        sum: [ 'incidH_1dose', 'incidH_0dose']
```
will create a new outcomes as the sum of the listed outcomes.

### Outcomes source from seir
The epidemic transmission model has some meta-compartment, e.g
- `infection_stage`: S, E, I1, I2, I3, R
- `vaccination_stage`: unvaccinated, 0dose, 1dose, 2dose
- `variant_type`: var0, var1, var2
where `infection_stage`,`vaccination_stage`,`variant_type` are the meta-compartments names.

The basic rational of source selection is that **everything that is not specified is summed across**. Let's go through some examples:

```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      incidH:
        source: 
          incidence:
            infection_stage: "I1"
            vaccination_stage: ["0dose", "1dose", "2dose"]
            variant_type: ["var0", "var1"]
```
will make a source time-series with the incidence in compartment I1 accross all `vaccination_stage`  compartments except unvaccinated, and only `variant_type` var0 and var1 (NOT var2).

```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      incidH:
        source: 
          incidence:
            infection_stage: "I1"
```
will put every incidence in `infection_stage` I1 accross every other meta-compartment. This is equivalent to a old CSP formulation that is kept for backward compatibility:
```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      incidH:
        source: incidI  # automatically sum for  `infection_stage`=I1 acrross every other meta-compartment
```

Instead of incidence, it is possible to select the prevalence in these compartments.


## Outcomes subclasses
TO BE WRITTEN

## Geographic parameter file
TO BE WRITTEN

## Outcomes NPIs
Outcomes NPIs can modify delay, duration and probabilities. The scenarios must be defined inside the `outcomes` subsection of the config:
```yaml
outcomes:
# [...]
  interventions:
      settings:
        high_death_rate:
          template: Stacked
          scenarios: ["times2H", "ICUprobability", "times2D"]
```
by default, the name of the parameter to be specified in the interventions is `$(outcome_name)::$(quantity)` where quantity is one of `duration`, `delay` and `probability`.

It is also possible to assign custom parameter names as follows:
```yaml
outcomes:
  method: delayframe
  param_from_file: False
  scenarios:
    - high_death_rate
  settings:
    high_death_rate:
      incidH:
        source: incidI
        probability:
          intervention_param_name: hosp_param_prob
          value:
            distribution: fixed
            value: .2
```
this is especially useful **when several outcomes shares the same NPIs values**.

The NPIs are defined as the epidemic transmission NPIs: inside the `npi:` block of the config.

Good examples are in configs:
- `gempyor_pkg/tests/outcomes/config_npi.yml`
- `gempyor_pkg/tests/outcomes/config_npi_custom_pnames.yml`

## Outcomes shape specification
Instead of point-delay, it is possible to specify a distribution (a _shape_) for the delay and durations of outcomes. A single incidence in the source is then spread according to the distribution, following a convolution operation (with some steps, see below.).

A good example config lies in `gempyor_pkg/tests/outcomes/config_shape_full.yml`.

To use this feature, instead of a `value:`, the name of a previously defined (see next section) shape is given after `shape: `. As such, single shape may be re-used for several outcomes.
```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      incidH:
        source: incidI
        probability:
          value:
            distribution: fixed
            value: .2
        delay:
          shape: delay_hosp
        duration:
          shape: duration_hosp ## shape instead of value: \n distribution: fixed, ...
```
Here we define the delay to hospitalization and the duration in hospital as shapes. These shapes are defined as:

### Shape definition
Shapes are defined in the root section `outcomes_shapes:` of the config. Technically, each shape is _the part in the present and the futur of a convolution kernel_. Intuitively, if the shape for a delay is  [.5, .25, .25] it means that half of the source incidence is an outcome incidence the same day, and a quarter is an outcome incidence the two following days. 

Here are some examples of shapes:
```yaml
outcomes_shapes:
  delay_hosp: # has len 11: [0.0548838 , 0.07270922, ... 0.07270922, 0.0548838 ]
      distribution: truncnorm
      mean: 10
      sd: 4
      a: 0
      b: 10
      intervention_param_name:
        mean: this_npi_affects_the_mean
        sd: this_npi_affects_the_sd
  duration_hosp: # gets normalized to [0, 0.0555, 0.111, 0.222, 0.222, 0.222, 0.111, 0.0555], see below
      array: [0, .05, .1, .2, .2, .2, .1, .05]
  duration_icu:  # has len(19). rougly  [0 , 0 , 0, 0.04304299, 0.04733261, 0.05153182, ..., 0.06784, 0.0655, 0.0626]
      distribution: truncnorm
      mean: 10
      sd: 10
      a: 0
      b: 20
      cutoff: 15
      shift: 3
  test_automatic_support:  # here the automatic cutoff cuts it after something like 45 days
    distribution: poisson
    lam: 30
  gamma_rate:
    distribution: gamma
    shape: 3
    rate: 1/10
  gamma_scale:
    distribution: gamma
    shape: 3
    scale: 10
```
The convolutions kernels (or shapes) can be defined as an array taken as it is, like `duration_hosp` above. This is a simple way to accomodate non-defined shapes, but it is not compatible with modifiers such as NPI (only `shift` works, see below).

Or if a functional form is defined, it is possible to use a distribution. In addition to the distribution specifications and parameters, there are three other parameters possible:
- `intervention_param_name`: for each distribution parameters, how they can be affected with NPIs **DOES NOT WORK YET**
- `cutoff`: by default, a automatic cutoff is defined the day after 99% of the point-mass of the distribution is passed. But this variable override the automatic cutoff with something longer or shorter. **It is recommended to use it in order to know when the outcomes output files becomes partial: at which point near the final time the mass is not conserved due to the truncation**. If the cutoff is 5 the (future and present part of the) convolution kernel will have lenght 5+1=6 (having the distribution pdf at time 0, 1, 2, 3, 4, 5) ! We assume the user expects the a convolution of length 5 in the futur (so 6 with the present.)
- `shift`: allows to shift the distribution in the futur by the specified amount. This is applied after all above transformation and thus not affected by the cutoff. we add `shift` zeros to the left of the convolution kernel. 

Gempyor will normalize the kernels to ensure that their mass (area under the curve) is 1. Moreover, gempyor will raise an error if negative number find their ways into the kernel.




#### technically: 
Moreover, as the user provides the distribution **in the future only**, to get an input suitable for convolution, gempyor appends `len(kernel)` zeros at the start of the kernel. So gempyor builds a convolution kernel with an odd number as length. Say the user give 6 values (a,b,c,d,e,f) because our user defined the cutoff as 5, then the convolution kernel is `0 0 0 0 0 a b c d e f` where a multiplies today value (and the padded zeros ensure we don't affect the past).

 **The first number in the shape definition is the number of individual entering this outcome (or exiting for a duration) the day of the source incidence**. 
 
It’s important to keep in mind that you are not specifying the convolution kernel but the present and futur part of it. E.g if you give a shape of [.5, .25, .25] for the delay between incidI to incidH, then for 4 individual in incidI on a certain day, 2 will go to incidH the same day, 1 the second day and 1 the third day (i.e the convolution kernel is [0,0,5, .25, .25]

#### Available distributions
Gempyor distributions are the same for NPIs, parameters, and outcomes shape:
- `fixed`
    - `value`
- `uniform`
    - `low`
    - `high`
- `poisson`
    - `lam`
- `binomial`
    - `n`
    - `p`
- `truncnorm`
    - `mean` of the normal, will not be the mean of the truncated normal !!!
    - `sd`
    - `a` lower bound
    - `b` upper bound
- `lognorm
    - `meanlog`
    - `sdlog`
- `gamma`:
    - `shape`
    - `scale` or `rate`s

Want another distribution?  ask for it, it's much easier if it is in this list https://docs.scipy.org/doc/scipy/reference/stats.html but we can arrange it. 


### Modifying shapes in time
W.I.P
