import itertools

from fractions import Fraction


class PlaneGroup(object):

    FUEL_CAPACITY = Fraction(1, 1)

    def __init__(
            self, quantity=1, fuel=None, position=Fraction(0, 1),
            returning=False
    ):
        self.quantity = quantity
        self.fuel = fuel or quantity * Fraction(1, 1)
        self.position = position
        self.returning = returning

    def split(self, quantity=1, fuel=Fraction(0, 0)):
        new = type(self)(quantity, fuel, self.position, self.returning)
        remaining = type(self)(
            quantity=self.quantity - quantity, fuel=self.fuel - fuel,
            position=self.position, returning=self.returning
        )
        return new, remaining

    def split_to_next_max(self):
        desired_remaining_fuel = (self.quantity - 1) * self.FUEL_CAPACITY
        turnback_fuel = self.fuel - desired_remaining_fuel
        return self.split(fuel=turnback_fuel)

    def join(self, other):
        assert self.position == other.position
        return type(self)(
            quantity=self.quantity + other.quantity, fuel=self.fuel + other.fuel,
            position=self.position, returning=other.returning,
        )

    @property
    def effective_position(self):
        if self.returning:
            return self.position * -1

    @property
    def effective_fuel(self):
        if self.returning:
            return (self.position + self.FUEL_CAPACITY) * self.quantity

    def distance_to_next_turnback(self, support=None):
        desired_remaining_fuel = (self.quantity - 1) * self.FUEL_CAPACITY
        if self.fuel < desired_remaining_fuel:
            return Fraction(0, 1)

        if support is None:
            return (
                (self.fuel - self.position - desired_remaining_fuel)
                /
                (self.quantity + 1)
            )

        fuel_needed_for_turnback = (self.position - support.effective_position)/2
        target_fuel_at_turnback = (
            desired_remaining_fuel + fuel_needed_for_turnback
        )
        turnback_to_get_to_support = (self.fuel - target_fuel_at_turnback) / self.quantity
        return turnback_to_get_to_support

    def _advance_to_refuel(self, exact=True):
        fuel_needed = self._fuel_needed_for_distance(self.position)
        if exact and fuel_needed != self.fuel:
            raise Exception("Inexact refuel")
        return type(self)(
            quantity=self.quantity, fuel=self.quantity*self.FUEL_CAPACITY
        )

    def _fuel_needed_for_distance(self, distance):
        fuel_needed = distance * self.quantity
        if self.fuel >= fuel_needed:
            return self.fuel - fuel_needed
        raise Exception("Not enough fuel")

    def advance(self, distance):
        if self.returning:
            if distance < self.position:
                new_fuel = self.fuel - self._fuel_needed_for_distance(distance)
                new_position = self.position - distance
                return type(self)(
                    fuel=new_fuel, position=new_position, returning=True,
                    quantity=self.quantity,
                )
            else:
                return self._advance_to_refuel().advance(distance-self.position)

        new_fuel = self.fuel - self._fuel_needed_for_distance(distance)
        new_position = self.position + distance
        return type(self)(
            fuel=new_fuel, position=new_position, quantity=self.quantity,
        )


def run_simulation(plane_count):
    return SimulationState(PlaneGroup(quantity=plane_count), []).simulate()


class SimulationState(object):

    def __init__(self, destination_group, support_groups):
        self.destination_group = destination_group
        self.support_groups = support_groups

    @property
    def supports_by_advancement(self):
        return sorted(
            self.support_groups, key=lambda group: group.effective_position,
            reverse=True,
        )

    @property
    def returning_supports(self):
        return (support for support in self.support_groups if support.returning)

    @property
    def nearest_support(self):
        s = self.supports_by_advancement
        if s:
            return s[0]

    @property
    def min_time_to_support_collision(self):
        min_time_to_collision = float('inf')
        for returning in self.returning_supports:
            for support in self.support_groups:
                distance_between_supports = returning.position - support.effective_position
                time_to_collision = distance_between_supports/2
                meeting_position = time_to_collision + support.effective_position
                if meeting_position > 0:
                    if time_to_collision < min_time_to_collision:
                        min_time_to_collision = time_to_collision
        return min_time_to_collision

    @property
    def min_turnback_time(self):
        min_time_to_turnback = float('inf')
        turnback_group = self.destination_group
        for support in self.supports_by_advancement:
            time_to_turnback = turnback_group.distance_to_next_turnback(support)
            if min_time_to_turnback > time_to_turnback:
                min_time_to_turnback = time_to_turnback
            turnback_group = support

        # Check the last group
        time_to_turnback = turnback_group.distance_to_next_turnback(None)
        if min_time_to_turnback > time_to_turnback:
            min_time_to_turnback = time_to_turnback
        return min_time_to_turnback

    def _advance(self, distance):
        return type(self)(
            self.destination_group.advance(distance),
            [support.advance(distance) for support in self.support_groups],
        )

    def simulate(self):
        support_returns = [
            support.position
            for support in self.support_groups if support.returning
        ]
        next_event = min(
            support_returns + [self.min_time_to_support_collision, self.min_turnback_time]
        )
        self._advance(next_event)
        nearest_support_return = min(support_returns)
        return self._advance(nearest_support_return).simulate()

        return self._advance(turnback_distance)._start_turnback()
