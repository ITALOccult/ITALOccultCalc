using System;

namespace Occult.Asteroids
{
	internal class AsteroidClass : IComparable
	{
		private int number;

		private string astClass;

		public int Number
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

		public string AstClass
		{
			get
			{
				return astClass;
			}
			set
			{
				astClass = value;
			}
		}

		internal void ReadCSVline(string InLine)
		{
			string[] array = InLine.Split(new char[1] { ',' });
			Number = int.Parse(array[0]);
			AstClass = array[1];
		}

		public int CompareTo(object other)
		{
			if (Number == ((AsteroidClass)other).Number)
			{
				return AstClass.CompareTo(((AsteroidClass)other).AstClass);
			}
			return Number.CompareTo(((AsteroidClass)other).Number);
		}

		public override string ToString()
		{
			return Number + "," + AstClass;
		}
	}
}
