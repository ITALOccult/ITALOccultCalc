using System;

namespace Occult.Asteroid_Observations
{
	internal class SmallDias : IComparable
	{
		private double dia;

		private string label;

		public string Label
		{
			get
			{
				return label;
			}
			set
			{
				label = value;
			}
		}

		public double Dia
		{
			get
			{
				return dia;
			}
			set
			{
				dia = value;
			}
		}

		public int CompareTo(object other)
		{
			return Dia.CompareTo(((SmallDias)other).Dia);
		}
	}
}
