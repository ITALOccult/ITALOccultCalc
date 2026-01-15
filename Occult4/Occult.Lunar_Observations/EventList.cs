using System;

namespace Occult.Lunar_Observations
{
	internal class EventList : IComparable
	{
		private string occevent;

		internal static int SortField;

		public string Occevent
		{
			get
			{
				return occevent;
			}
			set
			{
				occevent = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortField == 0)
			{
				return Occevent.Substring(0, 17).CompareTo(((EventList)other).Occevent.Substring(0, 17));
			}
			if (((EventList)other).Occevent.Substring(81, 11) == Occevent.Substring(81, 11))
			{
				return Occevent.Substring(0, 17).CompareTo(((EventList)other).Occevent.Substring(0, 17));
			}
			return Occevent.Substring(81, 11).CompareTo(((EventList)other).Occevent.Substring(81, 11));
		}
	}
}
