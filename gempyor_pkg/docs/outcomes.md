# Outcomes

Recall: A classical outcome is specified by:
- it's source (either from seir or another previously defined outcome)
- it's probability, which are taken as binomial draws from the source if stochastic simulations are activated, or otherwise just as proportions
- it's delay between the incidence in the source and the incidence in the outcome
- (not necessary) it's duration: in which case a new outcome is created (with name $(OUTCOME_NAME)_curr or the specified name with `name:` inside the duration). This new outcome stores the prevalence in this outcomes.

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

A non-classical outcome can be the sum of previously defined outcomes:
```yaml
outcomes: 
  settings:
    my_outcome_scenario:
      # [...] define outcomes incidH_1dose & incidH_0dose 
      incidH_from_sum:                                              
        sum: [ 'incidH_1dose', 'incidH_0dose']
```
will create a new outcomes as the sum of the listed outcomes.

### Outcomes source

## Outcomes subclasses


## Outcomes NPIs


## Outcomes shape specification
Instead of point-delay, it is possible to specify a distribution (a _shape_) for the delay and durations of outcomes. A single incidence in the source is then spread according to the distribution, following a convolution operation.

A good example config lies in `gempyor_pkg/tests/outcomes/config_shape_full.yml`

To use this feature, instead of a `value:`, the name of a previously defined (see next section) shape name is given after `shape`. A single shape may be re-used for several outcomes.
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
Shapes are defined in the root section `outcomes_shapes:` of the config. Each shape is a convolution kernel.
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
```
The convolutions kernels (or shapes) can be defined as an array taken as it is, like `duration_hosp` above. This is a simple way to accomodate non-defined shapes, but it is not compatible with modifiers such as NPI (only `shift` works, see below)

Or if a functional form is defined, it is possible to use a distribution. In addition to the distribution specifications and parameters, there are three other parameter possible:
- `intervention_param_name`: for each distribution parameters **DOES NOT WORK**
- `cutoff`: by default, a automatic cutoff is defined the day after the 99% of the point-mass of the distribution. But this setting overide the cutoff with something longer or shorter. It is recommended to use it in order to know when the outcomes files becomes partial. If the cutoff is 5 the convolution kernel will have lenght 5+1=6 (having the distribution pdf at time 0, 1, 2, 3, 4, 5) ! We assume the user expects the a convolution of length 5 in the futur (so 6 with the present.)
- `shift`: allow to shift the distribution in the futur by the specified amount. This is applied after all above transformation and thus not affected by the cutoff. we add `shift` zeros to the left of the convolution kernel. 

Gempyor will normalize the kernels to ensure that their mass (area under the curve) is 1. Moreover, gempyor will raise an error if negative number find their ways into the kernel.

Moreover, as the user provides the distribution **in the future only**, to get an input suitable for convolution, gempyor appends `len(kernel)` zeros at the start of the kernel.

So **The first number in the shape definition is the number of individual entering this outcome (or exiting for a duration) the day of the source incidence**.

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
Not currently working.
