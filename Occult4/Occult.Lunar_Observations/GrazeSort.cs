using System;

namespace Occult.Lunar_Observations
{
	internal class GrazeSort : IComparable
	{
		private string dateField;

		private string observation;

		public string DateField
		{
			get
			{
				return dateField;
			}
			set
			{
				dateField = value;
			}
		}

		public string Observation
		{
			get
			{
				return observation;
			}
			set
			{
				observation = value;
			}
		}

		public int CompareTo(object other)
		{
			return DateField.CompareTo(((GrazeSort)other).DateField);
		}
	}
}
