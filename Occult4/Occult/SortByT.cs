using System;

namespace Occult
{
	internal class SortByT : IComparable
	{
		private string eventDate = "";

		private int record;

		public string EventDate
		{
			get
			{
				return eventDate;
			}
			set
			{
				eventDate = value;
			}
		}

		public int Record
		{
			get
			{
				return record;
			}
			set
			{
				record = value;
			}
		}

		public int CompareTo(object other)
		{
			return EventDate.CompareTo(((SortByT)other).EventDate);
		}
	}
}
