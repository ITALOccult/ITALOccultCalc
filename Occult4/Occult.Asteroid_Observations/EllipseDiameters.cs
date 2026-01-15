using System;

namespace Occult.Asteroid_Observations
{
	internal class EllipseDiameters : IComparable
	{
		private double ellipseDiameter;

		private double ellipseUncertainty;

		private double satelliteDiameter;

		private double satelliteUncertainty;

		private string asteroidName = "";

		private int quality;

		private int numChords;

		private int astNumber;

		private bool solve_Circular = true;

		private bool solveAssumed = true;

		public int AsteroidNumber
		{
			get
			{
				return astNumber;
			}
			set
			{
				astNumber = value;
			}
		}

		public string AsteroidName
		{
			get
			{
				return asteroidName;
			}
			set
			{
				asteroidName = value;
			}
		}

		public double EllipseDiameter
		{
			get
			{
				return ellipseDiameter;
			}
			set
			{
				ellipseDiameter = value;
			}
		}

		public double EllipseUncertainty
		{
			get
			{
				return ellipseUncertainty;
			}
			set
			{
				ellipseUncertainty = value;
			}
		}

		public double SatelliteDiameter
		{
			get
			{
				return satelliteDiameter;
			}
			set
			{
				satelliteDiameter = value;
			}
		}

		public double SatelliteUncertainty
		{
			get
			{
				return satelliteUncertainty;
			}
			set
			{
				satelliteUncertainty = value;
			}
		}

		public bool Solve_Circular
		{
			get
			{
				return solve_Circular;
			}
			set
			{
				solve_Circular = value;
			}
		}

		public bool SolveAssumed
		{
			get
			{
				return solveAssumed;
			}
			set
			{
				solveAssumed = value;
			}
		}

		public int Quality
		{
			get
			{
				return quality;
			}
			set
			{
				quality = value;
			}
		}

		public int NumChords
		{
			get
			{
				return numChords;
			}
			set
			{
				numChords = value;
			}
		}

		public int CompareTo(object other)
		{
			return AsteroidNumber.CompareTo(((EllipseDiameters)other).AsteroidNumber);
		}
	}
}
