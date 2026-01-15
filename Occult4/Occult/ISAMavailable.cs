using System;

namespace Occult
{
	internal class ISAMavailable : IComparable
	{
		internal int number;

		internal string line;

		internal string id;

		internal string Line
		{
			get
			{
				return line;
			}
			set
			{
				line = value;
				string[] array = value.Split(new char[1] { ',' });
				number = int.Parse(array[0]);
				id = array[1];
			}
		}

		internal int Number
		{
			get
			{
				return number;
			}
			set
			{
				number = value;
			}
		}

		public int CompareTo(object other)
		{
			return Number.CompareTo(((ISAMavailable)other).Number);
		}
	}
}
